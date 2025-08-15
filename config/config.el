;;; config.el -*- no-byte-compile: t; lexical-binding: t; -*- 
;; effectively we have packaged configuration as a 'local' elpaca package that we load such that emacs compiles the files under site-lisp
;; while keeping the files outside of it un-byte-compiled

;; add to load path
(add-to-list 'load-path (expand-file-name "site-lisp/" (file-name-directory load-file-name)))

;; register and load custom file
(setq custom-file (expand-file-name ".custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (if (or (eq system-type 'darwin)
          (eq system-type 'gnu/linux))
      (shell-command (format "touch %s" custom-file))
    (if (eq system-type 'ms-dos)
        (shell-command (format "type NUL > %s" custom-file))
      (error "Unknown system!")))
  (message ".custom.el not found, creating..."))
(load custom-file)

;; appearance settings
;; font settings
(when (display-graphic-p)
  (defvar ui-default-font-height 120)
  (defvar ui-default-variable-font-height 110)
  (set-face-attribute 'default nil :font "TX\-02" :height ui-default-font-height)
  (set-face-attribute 'fixed-pitch nil :font "TX\-02" :height ui-default-font-height)
  (set-face-attribute 'variable-pitch nil :font "SF Compact Text" :height ui-default-variable-font-height))

;; set frame internal margin
(setq default-frame-alist
      (append (list '(vertical-scroll-bars . nil)
                    '(internal-border-width . 8))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 8)

(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil
      widget-image-enable nil) ;; this is apparently for non-ugly buttons

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil
      sentence-end-double-space nil)

(setq-default display-line-numbers-width 3
              display-line-numbers-widen t
              word-wrap t
              truncate-lines t)

;; theme and associated settings
(add-to-list 'custom-theme-load-path (expand-file-name "site-lisp/" (file-name-directory load-file-name)))
(load-theme 'lambda-dark t)

;; line numbers
(setq display-line-numbers-type 'relative
      truncate-partial-width-windows nil
      global-text-scale-adjust-resizes-frames nil
      line-number-mode t
      column-number-mode t)

(dolist (mode
         '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode t))))

;; scrolling optimisations courtesy of doom-emacs
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount-horizontal 2)

;; UI packages
(use-package indent-bars
  :defer t
  :hook ((julia-mode) . indent-bars-mode)
  :config
  (setq indent-bars-pattern "."
        indent-bars-width-frac 0.2
        indent-bars-pad-frac 0.25
        indent-bars-color-by-depth nil
        indent-bars-highlight-current-depth '(:face default :blend 0.4)))

(use-package blackout
  :config (blackout 'auto-fill-mode))

;; editor settings
(require 'editor)

;; keybinds
(require 'escape)
(with-eval-after-load 'evil
  (require 'evil-god-state)

  ;; bind evil keys
  (keymap-set evil-motion-state-map "SPC" 'editor-leader-map)
  (keymap-set evil-normal-state-map "SPC" 'editor-leader-map)
  (evil-define-key nil editor-leader-map
    "b" #'switch-to-buffer
    "k" #'kill-current-buffer
    " " #'find-file
    "f" #'find-file-other-window
    "w" #'make-frame
    ";" #'execute-extended-command
    "B" #'ibuffer
    "t" #'tab-new
    "d" #'dired
    "1" #'delete-other-windows
    "0" #'delete-window
    "s" #'tab-switch
    "v" #'vundo))
(setq ibuffer-expert t)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; org configuration
(require 'org-config)
;; latex configuration
(require 'latex-config)

(provide 'config)
;;; config.el ends here
