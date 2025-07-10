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
(setq backup-directory-alist `(("." . "~/.emacs-saves"))
      backup-by-copying t
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 4
      version-control t)

(setq-default indent-tabs-mode nil
	          tab-width 4
	          fill-column 80)

;; save customisations elsewhere
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

;; Consider using meow? but I want to maintain familiarity with vim bindings, the reverse motions don't seem interesting to me...
(setq help-window-select t
      comment-multi-line t
      kill-do-not-save-duplicates t
      comment-empty-lines t
      lazy-highlight-initial-delay 0)

;; open PDFs in zathura instead of DocView
;; but only on linux
(when (eq system-type 'gnu/linux)
  (defun editor-zathura-pdf-open ()
    (start-process "zathura" nil "zathura" "--fork" (buffer-file-name))
    (kill-buffer))
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . editor-zathura-pdf-open)))

;;; PACKAGES
(use-package evil
  :demand t
  :preface (setq evil-want-keybinding nil)
  :custom
  (evil-want-integration t)
  (evil-want-C-i-jump t)
  (evil-search-module 'evil-search "use vim-like search instead of isearch.")
  :init
  (setq evil-undo-system 'undo-fu)
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
  (which-key-enable-god-mode-support)
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
  (require 'smartparens-latex)

  (sp-with-modes 'org-mode
    (sp-local-pair "$" "$" :trigger "$")
    ;; (sp-local-pair "$" "$")
    ;; (sp-local-pair "" "" :actions '(rem))
    ;; (sp-local-pair "=" "=" :actions '(rem))
    ;; (sp-local-pair "" "" :actions '(rem))
    (sp-local-pair "\\left(" "\\right)" :trigger "\\l(" :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\left[" "\\right]" :trigger "\\l[" :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\left\\{" "\\right\\}" :trigger "\\l{" :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\left|" "\\right|" :trigger "\\l|" :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "(" ")")
    (sp-local-pair "\\(" "\\)")
    (sp-local-pair "\\[" "\\]")))

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

(use-package cape
  :defer t
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

;; allows out-of-order regex completion and iterative selection
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

(use-package evil-matchit
  :defer t
  :after (evil)
  :config
  (global-evil-matchit-mode 1))

(use-package evil-snipe
  :defer t
  :after (evil)
  :init
  ;; attach hooks
  (add-hook 'prog-mode-hook 'turn-on-evil-snipe-mode)
  (add-hook 'text-mode-hook 'turn-on-evil-snipe-mode)
  (add-hook 'prog-mode-hook 'turn-on-evil-snipe-override-mode)
  (add-hook 'text-mode-hook 'turn-on-evil-snipe-override-mode)
  :config
  (blackout 'evil-snipe-local-mode)

  ;; options
  (setq evil-snipe-repeat-scope 'visible
        evil-snipe-spillover-scope 'visible))

(use-package helpful
  :defer t
  :init
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h .") #'helpful-at-point))

(use-package god-mode
  :defer t
  :after evil
  :config
  (blackout 'god-local-mode))

; (use-package undo-tree
;   :defer t)

(use-package undo-fu
  :defer t
  :after evil)

(use-package vundo
  :defer t
  :after undo-fu
  :preface (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package vterm
  :disabled
  :defer t)

;;; HELPER FUNCTIONS
;; reload buffer contents with no confirmation
(defun revert-buffer-noconfirm ()
  "Revert buffer without confirmation"
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))


;; (define-key minibuffer-local-filename-completion-map
;;             [C-Backspace] #'find-file-test)

(provide 'editor)
;;; editor.el ends here
