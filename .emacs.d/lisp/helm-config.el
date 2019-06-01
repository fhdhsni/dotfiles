(use-package helm
  :bind (("C-c C-~" . helm-register)))
;; :diminish helm-mode
;; :commands R
;; :bind (("C-x C-u" . helm-M-x)
;;        ("M-x" . helm-M-x)
;;        ("C-h a" . helm-apropos)
;;        ("C-x C-b" . helm-buffers-list)
;;        ("C-x b" . helm-mini)
;;        ("C-x C-f" . helm-find-files)
;;        ("C-x C-n" . helm-occur)
;;        ("M-y" . helm-show-kill-ring)
;;        ("C-h SPC" . helm-all-mark-rings)
;;        ("C-h x" . helm-register)
;;        ("C-h D" . helm-info)
;;        ("C-h t" . helm-top)
;;                                       ; ("C-c h M-:" . helm-eval-expression-with-eldoc)
;;        ("M-I" . helm-semantic-or-imenu))
;; :config
;; (helm-mode t)
;; (global-set-key (kbd "C-,") 'helm-command-prefix)

;; (setq helm-locate-command
;;       "locate %s -e -A --regex %s -d ~/.externalharddisk.db: -n 100")
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;; (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
;; )

;; (use-package helm-config
;;   :after helm
;;   :config
;;   (setq helm-idle-delay 0.0
;;         helm-exit-idle-delay 0.0
;;         helm-locate-fuzzy-match nil
;;         helm-M-x-fuzzy-match t
;;         helm-buffers-fuzzy-matching t
;;         helm-recentf-fuzzy-match t
;;         helm-semantic-fuzzy-match t
;;         helm-imenu-fuzzy-match    t
;;         helm-apropos-fuzzy-match t
;;         helm-lisp-fuzzy-completion t
;;         helm-input-idle-delay 0.01
;;         helm-quick-update t
;;         helm-candidate-number-limit 50
;;         helm-buffers-favorite-modes '("js2-mode" "html-mode" "elixir-mode")
;;         helm-ff-auto-update-initial-value t
;;         helm-M-x-requires-pattern nil
;;         helm-ff-skip-boring-files t)
;;   (global-unset-key (kbd "C-x c")))
;; (use-package helm-eshell
;;   :after helm
;;   :defer t
;;   :config
;;   (add-hook 'eshell-mode-hook
;;             #'(lambda ()
;;                 (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history))))

;; (use-package helm-descbinds
;;   :disabled t
;;   :ensure t
;;   :defer t
;;   ;; :after helm
;;   :bind (("C-h b" . helm-descbinds)))

;; (use-package helm-dash
;;   :disabled t
;;   :defer t
;;   :ensure t
;;   ;; :after helm
;;   :commands (helm-dash)
;;   :chords ((";s" . helm-dash))
;;   :config
;;   (defun node-js ()
;;     (interactive)
;;     (setq-local helm-dash-docsets '("NodeJS")))
;;   (add-hook 'js2-mode-hook 'node-js)
;;   (setq helm-dash-common-docsets '("JavaScript")))

;; (use-package helm-swoop
;;   :disabled t
;;   :defer t
;;   :ensure t
;;   ;; :after helm
;;   :bind
;;   (("M-s s" . helm-swoop-without-pre-input)
;;    ("M-s M-s" . helm-swoop))
;;   :config
;;   (setq helm-swoop-use-fuzzy-match t)
;;   (progn
;;     (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;;     (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)))

;; (use-package helm-ls-git
;;   :disabled t
;;   :defer t
;;   :ensure t
;;   ;; :after helm
;;   :chords ((";b". helm-browse-project)))

;; (use-package helm-projectile
;;   :disabled t
;;   :diminish projectile-mode
;;   :defer t
;;   :ensure t
;;   ;; :after helm
;;   :bind (("C-x C-a" . helm-projectile-find-file))
;;   :init
;;   (add-hook 'prog-mode-hook 'projectile-mode)
;;   (setq projectile-globally-ignored-modes
;;         (quote
;;          ("erc-mode"
;;           "help-mode"
;;           "completion-list-mode"
;;           "Buffer-menu-mode"
;;           "gnus-.*-mode"
;;           "occur-mode"
;;           "text-mode")))
;;   (setq projectile-keymap-prefix (kbd "C-x a"))
;;   :config
;;   (helm-projectile-on)
;;   (setq projectile-switch-project-action 'projectile-dired)
;;   (setq
;;    projectile-create-missing-test-files t
;;    projectile-completion-system 'helm
;;    projectile-enable-caching t))

;; (use-package helm-ag
;;   :disabled t
;;   :ensure t
;;   :defer t
;;   ;; :after helm
;;   :chords (("jq" . helm-ag-project-root))
;;   :config
;;   (setq helm-ag-base-command "ag --nocolor --nogroup"))

;; (use-package helm-css-scss
;;   :disabled t
;;   :defer t
;;   :ensure t)
