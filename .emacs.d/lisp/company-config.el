(use-package company
  :diminish company-mode
  :demand t
  :bind (("C-c C-y" . company-complete)
         ("C-c y" . company-complete)
         ("C-`" . company-lsp))
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'elixir-mode-hook 'company-mode)
  (add-hook 'eshell-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-elm)
  (use-package company-tabnine
    :defer t
    :config
    (setq company-tabnine-max-num-results 5)
    (add-to-list 'company-backends #'company-tabnine))

  (require 'company-dabbrev)
  (use-package company-lsp
    :ensure t
    :init
    (push 'company-lsp company-backends)
    (setq company-lsp-cache-candidates t
          company-lsp-async t))
  (bind-keys
   :map company-active-map
   ;; ("M-p" . nil)
   ;; ("M-n" . nil)
   ;; ("C-m" . nil)
   ;; ("C-h" . nil)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("<tab>" . company-complete-common)
   ("C-t" . company-show-doc-buffer))

  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9)))

  (setq
   company-dabbrev-downcase nil
   company-tooltip-flip-when-above t
   company-minimum-prefix-length 3
   company-idle-delay 0.1
   company-selection-wrap-around t
   company-show-numbers t
   company-require-match 'never
   company-tooltip-idle-delay 0.5
   company-tooltip-align-annotations t)

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas nil
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package company-quickhelp
  :init
  (company-quickhelp-mode 1)
  :config
  (setq company-quickhelp-delay 0.8)
  (eval-after-load 'company
    '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)))
