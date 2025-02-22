;;; -*- lexical-binding: t; -*-

(use-package org
  :defer t
  :hook ((org-mode . org-cdlatex-mode) ;; enable cdlatex for quick writing inside the latex block
	     (org-mode . visual-line-mode) ;; wrap
	     ;; (org-mode . org-indent-mode)
         ) ;; show indents instead of multiple asterisks
  :config
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
