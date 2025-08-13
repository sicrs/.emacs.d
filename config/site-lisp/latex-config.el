;; latex-config.el -*- lexical-binding: t -*-
(use-package auctex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :custom
  (TeX-engine-alist '((default
                       "Tectonic"
                       "tectonic -X compile -f plain %T"
                       "tectonic -X watch"
                       nil)))

  (LaTeX-command-style '(("" "%(latex)")))
  (TeX-check-TeX nil)
  (TeX-engine 'default)
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura"))
  :config
  (with-eval-after-load 'tex
    (let ((tex-list (assoc "TeX" TeX-command-list))
          (latex-list (assoc "LaTeX" TeX-command-list)))
      (setf (cadr tex-list) "%(tex)"
            (cadr latex-list) "%l"))))

(use-package cdlatex
  :defer t
  :hook ((LaTeX-mode . cdlatex-mode)
         (LaTeX-mode . cdlatex-electricindex-mode))
  :bind (:map cdlatex-mode-map ("<tab>" . cdlatex-tab))
  :config
  (setq cdlatex-env-alist
        `(("equation*" "\\begin{equation*}\n?\n\\end{equation*}" nil)))
  (blackout 'cdlatex-mode))

(use-package latex-preview-pane
  :disabled
  :defer t)

(provide 'latex-config)
;; latex-config.el ends here
