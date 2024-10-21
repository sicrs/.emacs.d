;; keybinds.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <sicrs@gamma>
;; Maintainer:  <sicrs@gamma>
;; Created: October 13, 2024
;; Modified: October 13, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/sicrs/binds
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(defvar editor-escape-hook nil)

;; custom dwim escape adapted from doom
(defun editor-escape (&optional interactive)
  "Run `editor-escape-hook'"
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
	   (when interactive
	     (setq this-command #'abort-recursive-edit))
	   ((run-hook-with-args-until-success 'editor-escape-hook))
	   ((or defining-kbd-macro executing-kbd-macro) nil)
	   ((unwind-protect (keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'editor-escape)

(define-prefix-command 'editor-leader-map)
(with-eval-after-load 'evil
  ;; define a custom god mode
  (evil-define-state god
    "God state"
    :tag " <G> "
    :message "-- GOD MODE --"
    :entry-hook (evil-god-start-hook)
    :exit-hook (evil-god-stop-hook)
    :input-method t
    :intercept-esc nil)

  ;; define hooks
  (defun evil-god-start-hook ()
    "Run before entering `evil-god-state'"
    (god-local-mode 1))

  (defun evil-god-stop-hook ()
    "Run before exiting `evil-god-state'"
    (god-local-mode -1))

  (defvar evil-execute-in-god-state-buffer nil)
  (defvar evil-god-last-command nil)
  (defun evil-god-fix-last-command ()
    "Change `last-command' to be the command before `evil-execute-in-god-state'"
    (setq last-command evil-god-last-command))

  (defun evil-stop-execute-in-god-state ()
    "Switch back to previous evil state"
    (unless (or (eq this-command #'evil-execute-in-god-state)
		(eq this-command #'universal-argument)
		(eq this-command #'universal-argument-minus)
		(eq this-command #'universal-argument-more)
		(eq this-command #'universal-argument-other-key)
		(eq this-command #'digit-argument)
		(eq this-command #'negative-argument)
		(minibufferp))
      (remove-hook 'pre-command-hook 'evil-god-fix-last-command)
      (remove-hook 'post-command-hook 'evil-stop-execute-in-god-state)
      (when (buffer-live-p evil-execute-in-god-state-buffer)
	(with-current-buffer evil-execute-in-god-state-buffer
	  (if (and (eq evil-previous-state 'visual)
		   (not (use-region-p)))
	      (progn
		(evil-change-to-previous-state)
		(evil-exit-visual-state))
	    (evil-change-to-previous-state))))
      (setq evil-execute-in-god-state-buffer nil)))

  (defun evil-execute-in-god-state ()
    "Execute the next command in God state."
    (interactive)
    (add-hook 'pre-command-hook #'evil-god-fix-last-command t)
    (add-hook 'post-command-hook #'evil-stop-execute-in-god-state t)
    (setq evil-execute-in-god-state-buffer (current-buffer))
    (setq evil-god-last-command last-command)
    (cond
     ((evil-visual-state-p)
      (let ((mrk (mark))
	    (pnt (point)))
	(evil-god-state)
	(set-mark mrk)
	(goto-char pnt)))
     (t
      (evil-god-state)))
    (evil-echo "Switched to god-state for the next command..."))

  ;; unconditionally exit evil-god state
  (defun evil-god-state-bail ()
    "Stop current God command and exit God state"
    (interactive)
    (evil-stop-execute-in-god-state)
    (evil-god-stop-hook)
    (evil-normal-state))

  ;; bind keys
  (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
  ;; (evil-define-key 'god evil-god-state-map (kbd "ESC") #'evil-force-normal-state)
  ;; (evil-define-key 'god global-map [escape] 'god-state-bail)
  (keymap-set evil-motion-state-map "SPC" 'editor-leader-map)
  (keymap-set evil-normal-state-map "SPC" 'editor-leader-map)
  (evil-define-key nil editor-leader-map
    "b" #'switch-to-buffer
    "k" #'kill-this-buffer
    " " #'find-file
    "f" #'find-file-other-window
    "w" #'make-frame
    ";" #'execute-extended-command
    "B" #'ibuffer))

(setq ibuffer-expert t)
(global-set-key (kbd "C-x C-b") #'ibuffer)

(provide 'keybinds)
