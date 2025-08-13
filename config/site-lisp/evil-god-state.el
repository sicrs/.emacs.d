;; evil-god-state.el -*- lexical-binding: t -*-
(define-prefix-command 'editor-leader-map)
;; define a custom god-mode
(evil-define-state god
  "God state"
  :tag " <G> "
  :message "-- GOD MODE --"
  :entry-hook (evil-god-start-hook)
  :exit-hook (evil-god-stop-hook)
  :input-method t
  :intercept-esc nil)

;; define hooks here
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

(defun evil-god-state-bail ()
  "stop current God command and exit God state"
  (interactive)
  (evil-stop-execute-in-god-state)
  (evil-god-stop-hook)
  (evil-normal-state))

;; bind keys
(evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
(evil-define-key 'visual global-map "," 'evil-execute-in-god-state)

(provide 'evil-god-state)
;; evil-god-state.el ends here
