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
(defvar init--gc-cons-thval (* 16 1024 1024)) ;; 16MB

;; HELPER functions
(defun cfg-load (filename)
  "Load a file at FILENAME, checking if it exists."
  (let ((user-init-file
         (expand-file-name filename
                           user-emacs-directory)))
    (when (file-exists-p user-init-file)
      (load user-init-file nil t))))

;; GC wizardry
;; set maximal
(setq gc-cons-threshold most-positive-fixnum)
;; cleanup post-startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold init--gc-cons-thval)))

(set-language-environment "UTF-8")
(setq default-input-method nil)
(setq load-prefer-newer t)
;; font-compacting can be resource intensive
(setq inhibit-compacting-font-caches t)


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
(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(setq frame-title-format "%b - emacs")

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

;; end of early-init.el
(provide 'early-init)
;;; early-init.el ends here
