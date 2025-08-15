;; org-config.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ui/org-mode--compute-prefixes ()
  "Compute prefix strings for regular text and headlines"

  (setq org-indent--heading-line-prefixes
        (make-vector org-indent--deepest-level nil))

  (setq org-indent--inlinetask-line-prefixes
        (make-vector org-indent--deepest-level nil))

  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))

  ;; the following is problematic if the org document contains no headings
  ;; this is fixed by concatenating the sequence/list produced by org-element-map
  ;; (if empty) with a 0
  (let* ((min-indent 5)
         (indent (+ 1 (seq-max
                       (cons 0 (org-element-map
                                   (org-element-parse-buffer) 'headline
                                 #'(lambda (item)
                                     (org-element-property :level item)))))))
         (indent (max indent min-indent)))

    (dotimes (n org-indent--deepest-level)
      (aset org-indent--heading-line-prefixes n
            (make-string
             (min indent (max 0 (- indent 1 n))) ?\s))
      (aset org-indent--inlinetask-line-prefixes n
            (make-string indent ?\s))
      (aset org-indent--text-line-prefixes n
            (make-string indent ?\s)))))

;;;###autoload
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
  :hook ((org-mode . org-cdlatex-mode)
         (org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :config
  (advice-add 'org-indent--compute-prefixes :override
              #'ui/org-mode--compute-prefixes)

  ;; UI related tweaks
  (setq-default line-spacing 1)
  (setq fill-column 72
        org-hide-leading-stars nil
        org-level-color-stars-only nil
        org-indent-mode-turns-on-hiding-stars nil
        header-line-format nil
        org-pretty-entities t
        org-hide-emphasis-markers t)

  (when (require 'org-num nil t)
    (setq org-num-skip-unnumbered t
          org-num-skip-footnotes t
          org-num-max-level 2
          org-num-face nil))

  (set-face-attribute 'org-level-1 nil
                      :family "TX\-02" :weight 'semi-bold)

  (dolist (face '(org-level-2 org-level-3 org-level-4 org-level-5
                              org-level-6 org-level-7 org-level-8))
    (set-face-attribute face nil :inherit 'org-level-1))

  (setq org-preview-latex-default-process 'imagemagick
        org-latex-pdf-process '("tectonic -X compile %f"))

  ;; agenda location
  (setq org-agenda-files '("~/org")
        org-log-done 'time
        org-return-follow-link t
        org-hide-emphasis-markers t)

  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

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
  
  ;; this doesn't quite seem to work
  ;; (add-hook 'org-mode
  ;;           (lambda ()
  ;;             ;; disable line numbers
  ;;             (display-line-numbers-mode 0)
  ;;             (add-hook 'evil-visual-state-entry-hook
  ;;                       (lambda ()
  ;;                         (when (derived-mode-p 'org-mode)
  ;;                           (display-line-numbers-mode 1)))
  ;;                       nil t)
  ;;             (add-hook 'evil-visual-state-exit-hook
  ;;                       (lambda ()
  ;;                         (when (derived-mode-p 'org-mode)
  ;;                           (display-line-numbers-mode 0)))
  ;;                       nil t)))
  
  (blackout 'visual-line-mode)
  (blackout 'org-indent-mode))



(provide 'org-config)
;; org-config.el ends here
