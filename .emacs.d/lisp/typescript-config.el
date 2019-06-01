(use-package typescript-mode
  :defer t
  :hook
  ;; (typescript-mode . lsp)
  (typescript-mode . flycheck-mode)
  (typescript-mode . tide-mode)
  :config
  (prettier-js-mode)
  )

(use-package tide
  :defer t
  :bind (:map tide-mode-map
              ("C-c r" . tide-rename-symbol)
              ("C-c d" . tide-documentation-at-point)
              ("C-c C-t" . tide-fix))
  :config
  ;; (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
  (setq typescript-auto-indent-flag nil)
  (setq typescript-indent-level 2)
  (setq tide-always-show-documentation t)
  (tide-hl-identifier-mode)
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (prettier-js-mode)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)
    (projectile-mode)
    (hs-minor-mode)
    (add-to-list 'company-dabbrev-code-modes 'tide-mode))

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)

  (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
  ;; (with-eval-after-load 'flycheck
  ;;   (flycheck-add-mode 'typescript-tslint 'tide-mode))

  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode))
(setq tide-format-options
      '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
