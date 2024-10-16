;;; ui.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <sicrs@gamma>
;; Maintainer:  <sicrs@gamma>
;; Created: October 13, 2024
;; Modified: October 13, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/sicrs/ui
;; Package-Requires: ((emacs "29.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; font settings
(defvar ui-default-font-size 120)
(defvar ui-default-variable-font-size 120)

(set-face-attribute 'default nil :font "Berkeley Mono-11")
(set-face-attribute 'fixed-pitch nil :font "Berkeley Mono-11")
(set-face-attribute 'variable-pitch nil :font "SF Compact Text-11")

(use-package almost-mono-themes
  :config
  (load-theme 'almost-mono-black t))

;; line number
;; (global-display-line-numbers-mode t)
;; enable line numbers for text and code instead of enabling globally
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)
(setq display-line-numbers-type 'relative)
(dolist (mode
         '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode t))))

;; do not wrap lines
(set-default 'truncate-lines t)

;; cursor
;; disable the blinking cursor
(blink-cursor-mode -1)
;; do blink the matching parentheses
(setq blink-matching-paren nil)
;; do not stretch the cursor to fit wide characters
(setq x-stretch-cursor nil)

;; do not render the cursor in non focused windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil) ;; see help for this variable

;; this is from the typewriter era, somehow
(setq sentence-end-double-space nil)

;; line number in modeline
(setq line-number-mode t
      column-number-mode t)

;; UI-related packages
(use-package doom-modeline
  :disabled
  :config
  (doom-modeline-mode)
  (setq doom-modeline-icon nil))

;; blackout
(use-package blackout
  :config (blackout 'auto-fill-mode))

;; Scrolling optimisations courtesy of doom
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the cursor more than N
      ;; lines past window edges.
      ;; This is especially slow in larger files during large-scale scrolling commands.
      ;; If kept over 100, the window is never automatically recentered, whereas 0 recenters too aggresively.
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount-horizontal 2)

(provide 'ui)
;;; ui.el ends here
