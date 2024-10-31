;;; -*- lexical-binding: t; -*-

(use-package julia-mode
  :defer t
  :hook (julia-mode . (lambda () (require 'latex))) 
  :init
  (autoload 'LaTeX-math-abbrev-prefix "latex" nil t)
  ;; (setq julia-automatic-latexsub nil
  ;;       julia-latexsub-greedy nil)
  )

;; it doesn't work...
(use-package julia-ts-mode
  :ensure (:host github :repo "dhanak/julia-ts-mode")
  :after (julia-mode)
  :config
  (julia-ts-mode-setup))

(use-package julia-repl
  :disabled
  :after (julia-mode))

(defun julia-ts-mode-setup ()
  (treesit-font-lock-recompute-features
   '(function variable) '(definition)))
(add-hook 'julia-ts-mode-hook #'julia-ts-mode-setup)

(provide 'julia)
