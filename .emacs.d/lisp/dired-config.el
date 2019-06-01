(use-package dired-sidebar
  :defer t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-use-all-the-icons nil))

(use-package dired
  :defer t
  :ensure nil
  :bind* ("C-x C-j" . dired-jump)
  :config
  ;; (add-hook 'dired-mode-hook 'dired-filter-group-mode)
  ;; (add-hook 'dired-mode-hook 'dired-filter-by-dot-files)
  (define-key dired-mode-map (kbd "r") 'fhd/open-in-external-app)
  (define-key dired-mode-map (kbd "RET") 'fhd/open-in-external-app)
  ;; (setq dired-listing-switches "-Ahl --time-style long-iso")
  ;; (setq dired-listing-switches "-Ahl")
  (setq dired-listing-switches "-laGh1v --group-directories-first")
  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-isearch-filenames t)
  (add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))

  (defun fhd/dired-get-size ()
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
        (message
         "Size of all marked files: %s"
         (progn
           (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
           (match-string 1))))))

  (define-key dired-mode-map (kbd "z") 'fhd/dired-get-size)
  ;; dired hide details <start>
  (defun fhd/dired-mode-setup ()
    "to be run as hook for `dired-mode'."
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'fhd/dired-mode-setup)
  ;; dired hide details <end>
  (setq dired-recursive-deletes (quote top))
  (define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "b") (lambda ()
                                         (interactive)
                                         (find-alternate-file "..")))
  ;; (add-hook 'dired-mode-hook #'dired-du-mode)
  (defadvice dired-copy-filename-as-kill (after dired-filename-to-clipboard activate)
    (with-temp-buffer
      (insert (current-kill 0))
      (shell-command-on-region (point-min) (point-max)
                               (cond
                                ((eq system-type 'cygwin) "putclip")
                                ((eq system-type 'darwin) "pbcopy")
                                (t "xsel -ib"))))
    (message "%s => clipboard" (current-kill 0)))
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-narrow
  :defer t
  :after dired
  :config
  (define-key dired-mode-map (kbd ",") 'dired-narrow))

(use-package dired-quick-sort
  :disabled t
  :after dired)

(use-package dired-du
  :after dired)

(use-package runner
  :defer t
  :after dired)

(use-package direx
  :after dired)

(use-package dired-filter
  :defer 20
  :after dired
  :config
  (setq dired-filter-group-saved-groups
        (quote
         (("default"
           ("Video" (extension  "mkv" "avi" "mp4" "webm"))
           ("Archives" (extension  "zip" "rar" "gz" "bz2" "tar"))
           ("PDF" (extension  "pdf"))
           ("TEX" (extension  "tex"))
           ("JS" (extension  "js"))
           ("VUE" (extension  "vue"))
           ("TS" (extension  "ts"))
           ("JSON" (extension  "json"))
           ("CSS" (extension  "css"))
           ("Elixir" (extension  "ex" "exs"))
           ("HTML" (extension  "html"))
           ("PHP" (extension  "php"))
           ("Org" (extension  "org"))
           ("Java" (extension  "java"))
           ("LISP" (extension  "el"))
           ("TEXT" (extension  "txt"))
           ("TEXT" (extension  "svg"))
           ("SHELL" (extension  "sh"))
           ("MP3" (extension  "mp3"))
           ("IMG" (extension  "jpg" "png")))))))


;; ;;;;
;; ;; little modification to dired-mode that let's you browse through lots of files
;; (add-hook 'dired-mode-hook
;;   (lambda()
;;     (define-key dired-mode-map (kbd "C-o") 'dired-view-current)     ; was dired-display-file
;;     (define-key dired-mode-map (kbd "n")   'dired-view-next)           ; was dired-next-line
;;     (define-key dired-mode-map (kbd "p")   'dired-view-previous))) ; was dired-previous-line

;; (defun dired-view-next ()
;;   "Move down one line and view the current file in another window."
;;   (interactive)
;;   (dired-next-line)
;;   (dired-view-current))

;; (defun dired-view-previous ()
;;   "Move up one line and view the current file in another window."
;;   (interactive)
;;   (dired-previous-line)
;;   (dired-view-current))

;; (defun dired-view-current ()
;;   "View the current file in another window (possibly newly created)."
;;   (interactive)
;;   (if (not (window-parent))
;;       (split-window))                                   ; create a new window if necessary
;;   (let ((file (dired-get-file-for-visit))
;;         (dbuffer (current-buffer)))
;;     (other-window 1)                                          ; switch to the other window
;;     (unless (equal dbuffer (current-buffer))                 ; don't kill the dired buffer
;;       (if (or view-mode (equal major-mode 'dired-mode))   ; only if in view- or dired-mode
;;           (kill-buffer)))                                                    ; ... kill it
;;     (let ((filebuffer (get-file-buffer file)))
;;       (if filebuffer                              ; does a buffer already look at the file
;;           (switch-to-buffer filebuffer)                                    ; simply switch
;;         (view-file file))                                                    ; ... view it
;;       (other-window -1))))                   ; give the attention back to the dired buffer
