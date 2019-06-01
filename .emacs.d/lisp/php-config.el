(use-package php-mode
  :ensure t
  :defer t
  :init
  (setq flycheck-phpcs-standard "PSR2")
  (add-hook 'before-save-hook 'php-cs-fixer-before-save)
  :config
  (add-hook 'php-mode-hook
            '(lambda ()
               (require 'company-php)
               (set (make-local-variable 'company-backends)
                    '((php-extras-company company-dabbrev-code) company-capf company-files company-ac-php-backend))
               (ede-php-autoload-mode)
               (php-enable-psr2-coding-style)
               (company-mode)
               (flycheck-mode)
               ;; (ac-php-core-eldoc-setup) ;; enable eldoc
               (dumb-jump-mode)
               (setq prettify-symbols-alist '(("function" . ?ƒ) ("public" . ?φ) ("return" . ?⮐)))
               (prettify-symbols-mode)
               (setq tab-width 4
                  indent-tabs-mode nil
                  fill-column 80)
            (set (make-local-variable 'show-trailing-whitespace) t)
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
            (local-set-key (kbd "C-$") 'php-extras-insert-previous-variable)
            (local-unset-key (kbd "C-M-e"))
            ;; (local-set-key (kbd "C-.")  swiper)
            (local-unset-key (kbd "C-."))))

  (use-package phpcbf
    :ensure t
    :init
    (setq phpcbf-executable "/home/farhad/.config/composer/vendor/bin/phpcbf")
    ;; (add-hook 'php-mode-hook 'phpcbf-enable-on-save)
    (setq phpcbf-standard "PSR2"))

  (use-package ede-php-autoload
    :ensure t)

  (use-package php-extras
    :ensure t)

  (use-package phan
    :ensure t)

  (use-package geben
    :defer t
    :ensure t)

  (use-package php-cs-fixer
    :init
    (setq php-cs-fixer-rules-level-part-options (quote ("@PSR2")))
    :ensure t)

  (use-package php-refactor-mode
    :ensure t)

  (use-package company-php
    :ensure t)

  (use-package php-auto-yasnippets
    :ensure t)

  (use-package psysh
  :ensure t)

  (use-package php-eldoc
    :ensure t))
