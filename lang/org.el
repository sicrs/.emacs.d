;;; -*- lexical-binding: t; -*-

(use-package org
  :defer t
  :hook ((org-mode . org-cdlatex-mode) ;; enable cdlatex for quick writing inside the latex block
	 (org-mode . visual-line-mode) ;; wrap
	 (org-mode . org-indent-mode)) ;; show indents instead of multiple asterisks
  :config
  (setq org-preview-latex-default-process 'imagemagick)
  (setq org-latex-compiler "xelatex")

  ;; for org-modern -- this saves us the bother of calling the setq on hook
  (setq org-ellipsis "…")
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
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  ;; edit settings
  (setq org-auto-align-tags nil
	org-tags-column 0
	org-catch-invisible-edits 'show-and-error
	org-special-ctrl-a/e t
	org-insert-heading-respect-content t

	; Org styling
	org-hide-emphasis-markers t
	org-pretty-entities t

	; Agenda styling
	org-agenda-tags-column 0
	org-agenda-block-separator ?─))
