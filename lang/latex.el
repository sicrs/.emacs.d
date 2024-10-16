;;; latex.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <sicrs@gamma>
;; Maintainer:  <sicrs@gamma>
;; Created: October 14, 2024
;; Modified: October 14, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/sicrs/latex
;; Package-Requires: ((emacs "29.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(use-package auctex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :custom
  (TeX-engine 'xetex))

(use-package latex-preview-pane
  :defer t)

;; (use-package preview
;;   :after latex
;;   :hook ((LaTeX-mode . preview-larger-previews))
;;   :config
;;   (defun preview-larger-previews ()
;;     (setq preview-scale-funcion
;; 	  (lambda () (* 1.25
;; 			(funcall (preview-scale-from-face)))))))

(use-package cdlatex
  :defer t
  :hook ((LaTeX-mode . cdlatex-mode)
	 (LaTeX-mode . cdlatex-electricindex-mode))
  :bind (:map cdlatex-mode-map ("<tab>" . cdlatex-tab))
  :config
  (blackout 'cdlatex-mode))

(provide 'latex)
;;; latex.el ends here
