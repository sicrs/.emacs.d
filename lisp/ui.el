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
(when (display-graphic-p)
  (defvar ui-default-font-height 115)
  (defvar ui-default-variable-font-height 110)

  (set-face-attribute 'default nil :font "TX\-02-12" :height ui-default-font-height)
  (set-face-attribute 'fixed-pitch nil :font "TX\-02-12" :height ui-default-font-height)
  ;; (set-face-attribute 'variable-pitch nil :font "Iosevka Aile Medium-12" :height ui-default-variable-font-height)
  (set-face-attribute 'variable-pitch nil :font "SF Compact Text-12" :height ui-default-variable-font-height)
  ;; (set-face-font 'variable-pitch "Iosevka Aile Semibold-12")
  ;; (set-face-attribute 'mode-line nil :font "SF Compact Text-12" :weight 'regular)

  (with-eval-after-load 'org
    (set-face-attribute 'org-level-1 nil :font "SF Compact Text-15" :weight 'medium :height 180)))

(add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))

(use-package indent-bars
  :defer t
  :hook ((julia-mode) . indent-bars-mode)
  :config
  (setq indent-bars-pattern "."
        indent-bars-width-frac 0.2
        indent-bars-pad-frac 0.25
        indent-bars-color-by-depth nil
        indent-bars-highlight-current-depth '(:face default :blend 0.4)))

(setq modus-themes-bold-constructs t
      modus-themes-variable-pitch-ui nil
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-disable-other-themes t) ;
(load-theme 'modus-operandi t)		    ;
;; (load-theme 'modus-vivendi-tinted t)

;; line number
;; (global-display-line-numbers-mode t);:
;; enable line numbers for text and code instead of enabling globally
(setq-default display-line-numbers-width 3
	          display-line-numbers-widen t
	          word-wrap t
	          truncate-lines t)
(setq display-line-numbers-type 'relative
      truncate-partial-width-windows nil
      global-text-scale-adjust-resizes-frames nil)
(dolist (mode
         '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode t))))

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
