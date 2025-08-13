;; escape.el -*- lexical-binding: t; -*-

(defvar editor-escape-hook nil)

;; custom 'DWIM' escape adapted from doom-emacs
(defun editor-escape (&optional interactive)
  "Run `editor escape hook'"
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           (when interactive
             (setq this-command #'abort-recursive-edit))
           ((run-hook-with-args-until-success 'editor-escape-hook))
           ((or defining-kbd-macro executing-kbd-macro) nil)
           ((unwind-protect (keyboard-quit)))))))
  
(global-set-key [remap keyboard-quit] #'editor-escape)
(provide 'escape)
