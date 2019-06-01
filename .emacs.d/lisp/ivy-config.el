(use-package counsel
  :diminish ivy-mode
  :demand t
  :init
  (defun fhd/switch-buffer-show-name ()
    (interactive)
    (ivy-switch-buffer)
    (fhd/show-buffer-info))
  :bind* (
          ;; ("\C-x\C-n" . counsel-grep)
          ;; ("\C-x\C-n" . swiper)
          ("C-." . swiper)
          ("C-'" . ivy-switch-buffer)
          ("C-, C-r" . ivy-resume)
          ("C-, r" . counsel-recentf)
          ("C-, l" . counsel-linux-app)
          ("C-, m" . counsel-mark-ring)
          ("C-, t" . fhd/counsel-tldr)
          ("C-, c" . counsel-colors-web)
          ("C-, e" . counsel-esh-history)
          ("C-, C-f" . find-file-in-project)
          ("M-I" . counsel-imenu)
          ("C-x C-u" . counsel-M-x)
          ("C-x C-f" . counsel-find-file)
          ("C-h b" . counsel-descbinds)
          ("<f1> l" . counsel-find-library)
          ("<f2> i" . counsel-info-lookup-symbol)
          ("<f2> u" . counsel-unicode-char)
          ("C-, g" . counsel-git)
          ("C-, j" . counsel-git-grep)
          ("C-, a" . counsel-rg)
          ("C-, h" . counsel-apropos)
          ("C-, C-l" . counsel-locate)
          ("M-y" . counsel-yank-pop))
  :config
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
  (ivy-mode 1)
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  ;; (define-key isearch-mode-map (kbd "C-'") 'swiper-from-isearch)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s")
  (defun fhd/counsel-fzf-function (str)
    (if (< (length str) 3)
        (counsel-more-chars 3)
      (let ((cmd (format "fzf --no-sort --exact --filter='%s' | tac"
                         (counsel-unquote-regex-parens
                          (ivy--regex str)))))
        (message "%s" cmd)
        (counsel--async-command cmd))
      '("" "working...")))

;;;###autoload
  (defun fhd/counsel-fzf (&optional initial-input)
    (interactive)
    (ivy-read "Find: " #'fhd/counsel-fzf-function
              :initial-input initial-input
              :dynamic-collection t
              :history 'counsel-find-history
              :action (lambda (file)
                        (with-ivy-window
                          (when file
                            (find-file file))))
              :unwind #'counsel-delete-process
              :caller 'counsel-find))

  ;; locate useing externalharddisk
  (defun fhd/counsel-locate-function (str)
    (if (< (length str) 3)
        (counsel-more-chars 3)
      (let ((cmd (format "locate %s -e -A --regex %s -d ~/.externalharddisk.db: -n 100"
                         default-directory
                         (counsel-unquote-regex-parens
                          (ivy--regex str)))))
        (message "%s" cmd)
        (counsel--async-command cmd))
      '("" "working...")))

;;;###autoload
  (defun fhd/counsel-locate (&optional initial-input)
    (interactive)
    (ivy-read "Locate: " #'fhd/counsel-locate-function
              :initial-input initial-input
              :dynamic-collection t
              :history 'counsel-find-history
              :action (lambda (file)
                        (with-ivy-window
                          (when file
                            (find-file file))))
              :unwind #'counsel-delete-process
              :caller 'fhd/counsel-bash-find))

  ;; bash-find
  (defun fhd/counsel-bash-find-function (str)
    (if (< (length str) 3)
        (counsel-more-chars 3)
      (let ((cmd (format "find %s -iname '*%s*' | tac"
                         default-directory
                         (counsel-unquote-regex-parens
                          (ivy--regex str)))))
        (message "%s" cmd)
        (counsel--async-command cmd))
      '("" "working...")))

;;;###autoload
  (defun fhd/counsel-bash-find (&optional initial-input)
    (interactive)
    (ivy-read "Find: " #'fhd/counsel-bash-find-function
              :initial-input initial-input
              :dynamic-collection t
              :history 'counsel-find-history
              :action (lambda (file)
                        (with-ivy-window
                          (when file
                            (find-file file))))
              :unwind #'counsel-delete-process
              :caller 'fhd/counsel-bash-find))

  ;; (counsel-set-async-exit-code 'counsel-find 1 "Nothing found")
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  (defun fhd/counsel-evrything ()
    "list everything recursively"
    (interactive)
    (let* ((cands (split-string
                   (shell-command-to-string "find .") "\n" t)))
      (ivy-read "File: " cands
                :action #'find-file
                :caller 'fhd/counsel-everything)))

  (defun fhd/counsel-tldr ()
    "Search https://github.com/tldr-pages/tldr."
    (interactive)
    (let* ((default-directory "~/git/tldr")
           (cands (split-string
                   (shell-command-to-string
                    "git ls-files --full-name -- pages/")
                   nil t)))
      (ivy-read "Topic: " cands
                :action #'find-file
                :caller 'counsel-tldr)))

  (defvar ejmr/counsel-cheat-sh-history nil
    "History for `ejmr/counsel-cheat-sh'.")

  (defun ejmr/counsel-cheat-sh ()
    "Search `http://cheat.sh/' for help on commands and code."
    (interactive)
    (let ((url "http://cheat.sh/")
          ;; T - omit terminal sequences (no colors)
          ;;     Without that, we get this error:
          ;;       Too deeply nested to render properly; consider increasing
          ;;       `max-specpdl-size'.
          ;; q - quiet mode, don't show github/twitter buttons
          (options "?T&q"))
      (ivy-read "Search cheat.sh: "
                (process-lines "curl" "--silent" (concat url ":list" options))
                :require-match t
                :sort t
                :history 'ejmr/counsel-cheat-sh-history
                :action (lambda (input)
                          (eww-browse-url (concat url input options)))
                :caller 'ejmr/counsel-cheat-sh)))
  (defalias 'cheat.sh 'ejmr/counsel-cheat-sh)

  (defun modi/eww-rename-cheat-sh-buffer (&rest _)
    "Rename the `eww' buffer if it is showing a `cheat.sh' page."
    (let ((url (eww-copy-page-url)))
      (when (string-match "^http://cheat.sh/\\([^/?]+\\)" url)
        (rename-buffer (concat "*cheat.sh " (match-string 1 url) "*") :unique))))
  (advice-add 'eww :after #'modi/eww-rename-cheat-sh-buffer))

(use-package counsel-projectile
  :bind (("C-, C-f" . counsel-projectile-find-file)
         ("C-*" . counsel-projectile-find-file)
         ("C-, C-a" . counsel-projectile-ag)
         ("C-, C-b" . counsel-projectile-switch-to-buffer))
  :after counsel)

; (use-package ivy-explorer)

; (ivy-explorer-mode 1)
; (counsel-mode 1)
; (use-package ivy-rich
;   :init
;   (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
;   (setq ivy-rich-path-style 'abbrev)
;   :ensure t)


;; (use-package ivy-posframe
;;   :init
;;   (setq ivy-posframe-width (frame-width))
;;   (setq ivy-posframe-hide-minibuffer t)
;;   (setq ivy-posframe-parameters
;;         '((left-fringe . 2)
;;           (right-fringe . 2)))
;;   (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
;;   (ivy-posframe-enable)
;;   (custom-set-faces
;;  '(ivy-posframe ((t (:inherit default :background "#091A32" :foreground "#dcdccc"))))))
