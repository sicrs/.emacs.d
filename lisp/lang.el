;;; -*- lexical-binding: t; -*-

(defmacro load-language-module (path)
  "Load language module found in folder lang/"
  `(load (expand-file-name ,path user-emacs-directory)))

;; load latex
(load-language-module "lang/latex.el")
(load-language-module "lang/org.el")

(provide 'lang)
