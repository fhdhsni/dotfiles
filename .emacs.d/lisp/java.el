(require 'cc-mode)
(require 'dap-java)
(require 'lsp-java-treemacs)

(use-package lsp-java
  :ensure t
  :bind* ("C-c TAB" . lsp-java-add-import)
  :config
  (add-hook 'java-mode-hook 'lsp-ui-mode)
  (add-hook 'java-mode-hook 'lsp-java-enable)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'java-mode-hook 'company-mode)
  (add-hook 'java-mode-hook
	        (lambda ()
              (setq lsp-inhibit-message t)
              (setq lsp-highlight-symbol-at-point nil)
	          (setq-local company-backends (list 'company-lsp)))))

(use-package gradle-mode)
