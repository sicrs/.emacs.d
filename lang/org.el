;;; -*- lexical-binding: t; -*-

(defun ui/org-mode--compute-prefixes ()
  "Compute prefix strings for regular text and headlines"
  (setq org-indent--heading-line-prefixes
        (make-vector org-indent--deepest-level nil))

  (setq org-indent--inlinetask-line-prefixes
        (make-vector org-indent--deepest-level nil))

  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))

  (let* ((min-indent 5)
         (indent (+ 1 (seq-max
                       (org-element-map
                        (org-element-parse-buffer) 'headline
                        #'(lambda (item)
                            (org-element-property :level item))))))
         (indent (max indent min-indent)))

    (dotimes (n org-indent--deepest-level)
      (aset org-indent--heading-line-prefixes n
            (make-string
             (min indent (max 0 (- indent 1 n))) ?\s))
      (aset org-indent--inlinetask-line-prefixes n
            (make-string indent ?\s))
      (aset org-indent--text-line-prefixes n
            (make-string indent ?\s)))))

(defun ui/org-mode--num-format (numbering)
  "Alternative numbering format adapted from rougier's nano-emacs"

  (if (= (length numbering) 1)
      (propertize (concat (mapconcat
                           #'number-to-string
                           numbering ".") " | " )
                  'face `(:family "TX\-02"
                                  :height 250))
    (propertize (concat (mapconcat
                         #'number-to-string
                         numbering ".") " — " )
                'face `(:family "TX\-02"))))

(use-package org
  :defer t
  :hook ((org-mode . org-cdlatex-mode) ;; enable cdlatex for quick writing inside the latex block
	     (org-mode . visual-line-mode) ;; wrap
	     (org-mode . org-indent-mode)
         ;; (org-mode . org-num-mode)
         ) ;; show indents instead of multiple asterisks
  :config
  (advice-add 'org-indent--compute-prefixes :override
              #'ui/org-mode--compute-prefixes)

  ;; UI tweaks
  (setq fill-column 72)
  (setq-default line-spacing 1)
  (setq org-hide-leading-stars nil)
  (setq org-level-color-stars-only nil)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq header-line-format nil)
  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)

  (when (require 'org-num nil t)
    (setq org-num-skip-unnumbered t)
    (setq org-num-skip-footnotes t)
    (setq org-num-max-level 2)
    (setq org-num-face nil))
    ;; (setq org-num-format-function 'ui/org-mode--num-format))

  (set-face-attribute 'org-level-1 nil
                      :family "TX\-02" :weight 'semi-bold)

  (dolist (face '(org-level-2 org-level-3 org-level-4
                              org-level-5 org-level-6 org-level-7 org-level-8))
    (set-face-attribute face nil :inherit 'org-level-1))
  ;; (set-face-attribute 'org-level-2 nil
  ;;                     :family "TX\-02" :weight 'medium)
  ;; (set-face-attribute 'org-level-3 nil
  ;;                     :family "TX\-02" :weight 'medium)
 
  (setq org-preview-latex-default-process 'imagemagick
        org-latex-pdf-process '("tectonic -X compile %f"))

  ;; location for agenda
  (setq org-agenda-files '("~/org") ;; emacs now knows where to find the agenda
	    org-log-done 'time ;; record timestamp on completion
	    org-return-follow-link t ;; follow links using RET
	    org-hide-emphasis-markers t) ;; hide ** on bold

  ;; for org-modern -- this saves us the bother of calling the setq on hook
  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  ;; (set-face-attribute 'org-meta-line nil :family "SF Pro Text")

  ;; configure capture templates
  (setq org-todo-keywords
	    '((sequence  "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "BLOCKED(b@)" "|" "DONE(d!)" "ABORTED(w@/!)")))
  (setq org-capture-templates
	    '(("g" "Generic to-do"
	       entry (file+headline "~/org/todo.org" "General tasks")
	       "* TODO [#B] %?\n:Created: %T\n "
	       :empty-lines 0)
	      ))

  ;; custom latex-macros babel language for custom \newcommand
  (add-to-list 'org-src-lang-modes '("latex-macros" . latex))
  (defvar org-babel-default-header-args:latex-macros
    '((:results . "raw")
      (:exports . "results")))
  
  (defun prefix-all-lines (pre body)
    (with-temp-buffer
      (insert body)
      (string-insert-rectangle (point-min) (point-max) pre)
      (buffer-string)))
  
  (defun org-babel-execute:latex-macros (body _params)
    (concat
     (prefix-all-lines "#+LATEX_HEADER: " body)
     "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
     (prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
     "\n#+HTML_HEAD_EXTRA: \\)</div>\n"))
  
  (blackout 'visual-line-mode)
  (blackout 'org-indent-mode))

(use-package org-modern
  :disabled
  :hook ((org-mode . org-modern-mode)
	     (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-fold-stars '(("▶" . "▼")
			               ("▷" . "▽")
			               ("▸" . "▾")
			               ("▹" . "▿")
			               ("▸" . "▾")))
  :init
  (add-hook 'org-modern-mode-hook
	        (lambda ()
	          (setq line-spacing 0.3)))
  ;; edit settings
  (setq org-auto-align-tags nil
	    org-tags-column 0
	    org-catch-invisible-edits 'show-and-error
	    org-special-ctrl-a/e t
	    org-insert-heading-respect-content t

                                        ; Org styling
	    org-hide-emphasis-markers t
	    org-pretty-entities nil

                                        ; Agenda styling
	    org-agenda-tags-column 0
	    org-agenda-block-separator ?─)
  :config
  (set-face-attribute 'org-modern-symbol nil :family "STIX Two Text"))

;; (defun org--variable-pitch-set ()
;;   "Enable variable-pitch-mode and set line numbers face to fixed pitch or monospace]"
;;   (variable-pitch-mode 1)
;;   (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))
