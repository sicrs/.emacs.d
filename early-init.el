;; early-init.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <sicrs@gamma>
;; Maintainer:  <sicrs@gamma>
;; Created: October 11, 2024
;; Modified: October 11, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/sicrs/early-init
;; Package-Requires: ((emacs "29.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; Sometimes you have to deal with the arcane in emacs configuration

;;; VARIABLES
(defvar init--debug nil)
(defvar init--gc-cons-thval (* 16 1024 1024)) ;; 16MB
(defvar default-file-name-handler-alist file-name-handler-alist)

;; GC wizardry
;; set maximal
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1
      file-name-handler-alist nil)
(defun +gc-after-focus-change()
  "Run GC when frame loses focus"
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
	   gc-cons-percentage 0.1
	   gc-cons-threshold init--gc-cons-thval)
     (message "gc-cons-threshold and file-name-handler-alist restored")
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook '+reset-init-values))

(set-language-environment "UTF-8")
(setq default-input-method nil)
(setq load-prefer-newer t)
;; font-compacting can be resource intensive
(setq inhibit-compacting-font-caches t)

;; ignore X-resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; enable native and byte compilation
(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; enable native compilation
    (setq native-comp-jit-compilation t
          ;; native-comp-deferred-compilation t ;; this is obsolete since emacs 29.1
          package-native-compile t)
  ;; disable native compilation
  (setq features (delq 'native-compile features)))

;; UI optimisations
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      server-client-instructions nil
      frame-inhibit-implied-resize t
      auto-mode-case-fold nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(setq frame-title-format "%b - emacs")

;; forgo bidirectional functionality for a modest perf boost
(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(advice-add #'display-startup-echo-area-message :override #'ignore)

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(unless init--debug
  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))

;; disable file dialogs
(setq use-file-dialog nil
      use-dialog-box nil)

;; unless running as a daemon and non interactive
;; (unless (daemonp)
;;   (unless noninteractive
;;     (when (fboundp 'tool-bar-setup)
;;       ;; Temporarily override the tool-bar-setup function to prevent it from
;;       ;; running during the initial stages of startup
;;       (advice-add #'tool-bar-setup :override #'ignore)
;;       (define-advice startup--load-user-init-file
;;           (:after (&rest _) minimal-emacs-setup-toolbar)
;;         (advice-remove #'tool-bar-setup #'ignore)
;;         (when tool-bar-mode
;;           (tool-bar-setup))))))

;; necessary for elpaca.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)
(setq use-package-always-ensure nil) ;; does this even exist when I don't use use-package.el

(defun early-init-display-startup-time ()
  "Display startup time."
  (message "emacs loaded in %s with %d garbage collections"
           (format "%2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'early-init-display-startup-time)

;; HELPER functions
(defun cfg-load (filename)
  "Load a file at FILENAME, checking if it exists."
  (let ((user-init-file
         (expand-file-name filename
                           user-emacs-directory)))
    (when (file-exists-p user-init-file)
      (load user-init-file nil t))))

;; end of early-init.el
(provide 'early-init)
;;; early-init.el ends here
