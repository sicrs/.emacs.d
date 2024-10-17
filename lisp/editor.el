;;; editor.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <sicrs@gamma>
;; Maintainer:  <sicrs@gamma>
;; Created: October 13, 2024
;; Modified: October 13, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/sicrs/editor
;; Package-Requires: ((emacs "29.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; put savefiles elsewhere so it doesn't litter my folders
(setq backup-directory-alist `(("." . "~/.emacs-saves")))

;; save customisations elsewhere
(setq custom-file (expand-file-name ".custom.el" user-emacs-directory))
(load custom-file)

;; Consider using meow? but I want to maintain familiarity with vim bindings, the reverse motions don't seem interesting to me...
(setq help-window-select t)

;;; PACKAGES
(use-package evil
  :demand t
  :preface (setq evil-want-keybinding nil)
  :custom
  (evil-want-integration t)
  (evil-want-C-i-jump t)
  (evil-search-module 'evil-search "use vim-like search instead of isearch.")
  :config
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (if (minibufferp)
                  (evil-emacs-state))))
  (evil-global-set-key 'normal ";" #'evil-ex)
  (evil-global-set-key 'visual ";" #'evil-ex)

  ;; use the dwim escape defined below
  (defun evil-escape-a (&rest _)
    "Call custom DWIM escape if evil-force-normal-state is called interactively"
    (when (called-interactively-p 'any)
      (call-interactively #'editor-escape)))
  (advice-add #'evil-force-normal-state :after #'evil-escape-a)
  
  (evil-mode))

(use-package which-key
  :defer 0.1
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (blackout 'which-key-mode)
  (which-key-mode)
  :custom
  (which-key-init-delay 0.2)
  (which-key-side-window-max-width 0.33))

(use-package vertico
  :defer 1
  :init (vertico-mode))

(use-package smartparens
  :defer t
  :hook (prog-mode text-mode markdown-mode)
  :config
  (blackout 'smartparens-mode)
  (require 'smartparens-config)
  (require 'smartparens-latex))

(use-package corfu
  :defer 3
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :custom
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  :config
  (setq tab-always-indent 'complete)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (global-corfu-mode)
  (with-eval-after-load 'evil
    (setq evil-complete-next-func (lambda (_) (completion-at-point)))))

(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package evil-smartparens
  :defer t
  :after smartparens
  :hook (smartparens-mode)
  :config
  (blackout 'evil-smartparens-mode))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init)
  (blackout 'evil-collection-unimpaired-mode)
  :init (setq evil-collection-setup-minibuffer t))

(use-package evil-anzu
  :disabled
  :after evil
  :defer t)

(use-package helpful
  :defer t
  :init
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h .") #'helpful-at-point))

;; MISC relating to keymaps
;; custom DWIM escape, adapted from doom
(defvar editor-escape-hook nil
  "A hook run when C-g is pressed, or ESC in normal mode.")
(defun editor-escape (&optional interactive)
  "Run `editor-escape-hook'"
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           (when interactive
             (setq this-command #'abort-recursive-edit))
           (abort-recursive-edit))
          ((run-hook-with-args-until-success 'editor-escape-hook))
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'editor-escape)

;; TODO: clear
;; play around with find-file minibuffer completion
(defun find-file-test ()
  (interactive)
  (message "lmao"))

(define-key minibuffer-local-filename-completion-map
            [C-Backspace] #'find-file-test)

(provide 'editor)
;;; editor.el ends here
