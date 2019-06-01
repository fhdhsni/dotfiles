(use-package js2-mode
  :defer t
  :diminish Javascript-IDE
  :mode "\\.js\\'"
  :load-path "~/.nvm/versions/node/v10.12.0/lib/node_modules/tern/emacs"
  :hook ((js2-mode . lsp)
         (js2-mode . hs-minor-mode))
  :init
  ;; (autoload 'tern-mode "tern.el" nil t)
  (add-hook 'js2-mode-hook #'(lambda ()
                               (prettier-js-mode t)
                               ;; (tern-mode t)
                               (flycheck-mode t)
                               ;; (setq prettify-symbols-alist '(("function" . ?ƒ)))
                               ;; (prettify-symbols-mode t)
                               (setq js2-mode-show-parse-errors nil)
                               (setq js2-mode-show-strict-warnings nil)
                               (setq js2-basic-offset 2)
                               (setq js2-strict-trailing-comma-warning nil))))

(use-package indium :after js2-mode)

(use-package rjsx-mode :after js2-mode)

;; (use-package company-tern :after js2-mode)

(use-package js2-refactor
  :after js2-mode
  :diminish js2-refactor-mode
  :hook (js2-mode-hook . js2-refactor-mode))

(use-package eslint-fix :after js2-mode)
