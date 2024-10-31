;;; -*- lexical-binding: t; -*-
;; (use-package treesit-auto
;;   :defer t
;;   :custom (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(defmacro load-language-module (path)
  "Load language module found in folder lang/"
  `(load (expand-file-name ,path user-emacs-directory)))

;; load latex
(load-language-module "lang/latex.el")
(load-language-module "lang/org.el")

;; programming languages
(load-language-module "lang/julia.el")

(provide 'lang)
