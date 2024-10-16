;;; -*- lexical-binding: t; -*-

(use-package org
  :defer t
  :hook (org-mode . org-cdlatex-mode)
  :config
  (setq org-preview-latex-default-process 'imagemagick)
  (setq org-latex-compiler "xelatex"))
