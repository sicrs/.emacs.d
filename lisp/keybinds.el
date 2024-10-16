;;; keybinds.el --- Description -*- lexical-binding: t; -*-
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
(define-prefix-command 'editor-leader-map)

(with-eval-after-load 'evil
  (keymap-set evil-motion-state-map "SPC" 'editor-leader-map)
  (keymap-set evil-normal-state-map "SPC" 'editor-leader-map)
  (evil-define-key nil editor-leader-map
    "b" #'switch-to-buffer
    "k" #'kill-buffer
    " " #'find-file
    "f" #'find-file-other-window
    "w" #'make-frame
    ";" #'execute-extended-command
    "B" #'ibuffer))

(setq ibuffer-expert t)
(global-set-key (kbd "C-x C-b") #'ibuffer)

(provide 'keybinds)
