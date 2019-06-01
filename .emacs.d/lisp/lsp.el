(use-package lsp-mode
  :ensure t
  :commands lsp
  :bind* (("C-, d" . lsp-describe-thing-at-point))
  :config
  (flymake-mode 0)
  :init
  (require 'lsp-clients)
  (add-hook 'lsp-ui-mode-hook
	        (lambda ()
              (flymake-mode 0)
              (setq lsp-ui-sideline-enable nil)
              (setq lsp-ui-doc-enable nil)))
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-eldoc-render-all nil
        lsp-highlight-symbol-at-point nil)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "reason-language-server.exe")
                    :major-modes '(reason-mode)
                    :server-id 'ocaml-ls)))

(use-package lsp-ui
  :defer t
  :ensure t
  :config
  (setq lsp-ui-sideline-update-mode 'point))

(require 'lsp-ui-flycheck)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-after-open-hook (lambda ()
                                   (flymake-mode 0)
                                   (lsp-ui-flycheck-enable 1))))



;; (use-package dap-mode
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (dap-mode t))
