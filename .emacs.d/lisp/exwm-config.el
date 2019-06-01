;;; exwm-config.el --- configuration file for exwm

;;; Code:

(use-package xelb
  :demand t
  :ensure t)

(use-package exwm
  :ensure nil
  :demand t
  :load-path "~/git/exwm"
  :init
  (require 'exwm)
  ;; (require 'exwm-randr)
  (require 'exwm-systemtray)
  (require 'subr-x)  ;; Because of when-let
  (fringe-mode 1)
  (exwm-enable)
  ;; (exwm-randr-enable)
  (exwm-systemtray-enable)
  (setq exwm-layout-show-all-buffers nil)
  ;; (setq exwm-randr-workspace-output-plist '(0 "LVDS1"
  ;;                                             1 "VGA1"
  ;;                                             2 "VGA1"
  ;;                                             3 "VGA1"
  ;;                                             4 "VGA1"
  ;;                                             5 "VGA1"
  ;;                                             6 "VGA1"
  ;;                                             7 "VGA1"
  ;;                                             8 "VGA1"
  ;;                                             9 "VGA1"))
  ;; (add-hook 'exwm-randr-screen-change-hook
  ;;           (lambda ()
  ;;             (start-process-shell-command
  ;;              "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
  ;; (setq exwm-input-prefix-keys '(3 24 21 8 134217848 134217824 134217766 134217786)) ;Default
  (setq exwm-input-prefix-keys '(24 21 8 134217848 134217824 134217766 134217786))
  (setq exwm-workspace-number 10)
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; (add-hook 'exwm-init-hook
  ;;           (lambda ()
  ;;             (defvar fhd/exwm-workspace-previous-index nil "The previous active workspace index.")

  ;;             (defun fhd/exwm-workspace--current-to-previous-index (_x)
  ;;               (setq fhd/exwm-workspace-previous-index exwm-workspace-current-index))

  ;;             (advice-add 'exwm-workspace-switch :before #'fhd/exwm-workspace--current-to-previous-index)

  ;;             (defun fhd/exwm-workspace-switch-to-previous ()
  ;;               (interactive)
  ;;               "Switch to the previous active workspace."
  ;;               (let ((index fhd/exwm-workspace-previous-index))
  ;;                 (exwm-workspace-switch index)))))

  (defun fhd/copy-arrow-to-clipboard()
    (interactive)
    (kill-new "⟹")
    (message "arrow copied to clipboard"))

  (defun fhd/toggle-exwm-input-line-mode-passthrough ()
    (interactive)
    (if exwm-input-line-mode-passthrough
        (progn
          (setq exwm-input-line-mode-passthrough nil)
          (message "App receives all the keys now (with some simulation)"))
      (progn
        (setq exwm-input-line-mode-passthrough t)
        (message "emacs receives all the keys now")))
    (force-mode-line-update))

                                        ;make exwm windows default to char instead of line mode
  ;; (add-hook 'exwm-manage-finish-hook
  ;;           (lambda () (call-interactively #'exwm-input-release-keyboard)
  ;;             (exwm-layout-hide-mode-line)))

                                        ;send all keypresses to emacs in line mode
  (setq exwm-input-line-mode-passthrough nil)

  (defun fhd/exwm-input-line-mode ()
    "Set exwm window to line-mode and show mode line."
    (call-interactively #'exwm-input-grab-keyboard)
    (exwm-layout-show-mode-line))

  (defun fhd/exwm-input-char-mode ()
    "Set exwm window to char-mode and hide mode line."
    (call-interactively #'exwm-input-release-keyboard)
    (exwm-layout-hide-mode-line))

  (defun fhd/exwm-input-toggle-mode ()
    "Toggle between line- and char-mode."
    (interactive)
    (with-current-buffer (window-buffer)
      (when (eq major-mode 'exwm-mode)
        (if (equal (second (second mode-line-process)) "line")
            (fhd/exwm-input-char-mode)
          (fhd/exwm-input-line-mode)))))

  (defun fhd/dmenu ()
    (interactive)
    (shell-command "dmenu_run"))

  ;; ([?\C-x ?h] . ?\C-a)
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-y] . ?\C-v)
     ([?\M-b] . C-left)
     ([?\M-f] . C-right)
     ([?\C-s] . (?\C-f))
     ([?\M-w] . (?\C-c))
     ([?\C-w] . (?\C-x))
     ([?\M-@] . (S-C-right))
     ([?\M-d] . (C-delete))
     ([?\C-/] . (?\C-z))
     ([?\C-g] . escape)
     ([?\C-m] . return)
     ([?\C-o] . (return home up end))
     ([?\C-k] . (S-end delete))))

  (defvar exwm-workspace-window-assignments
    '(("Chromium" . 2)
      ("Firefox" . 2)
      ("mpv" . 3)
      ("Termite" . 0)
      ("konsole" . 0)
      ("Gnome-terminal". 0)
      ("TelegramDesktop" . 5)
      ("okular" . 7)
      ("Anki" . 8)
      ("GoldenDict" . 9))
    "An alist of window classes and which workspace to put them on.")


  ;; (setq exwm-manage-finish-hook nil)
  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              (cond
               ((string= exwm-class-name "konsole") (fhd/exwm-input-char-mode))
               ((string= exwm-class-name "code") (fhd/exwm-input-char-mode))
               ((string= exwm-class-name "Code") (fhd/exwm-input-char-mode))
               ((string= exwm-class-name "Gnome-terminal") (fhd/exwm-input-char-mode))
               ((string= exwm-class-name "sublime_text") (fhd/exwm-input-char-mode))
               ((string= exwm-class-name "Termite") (fhd/exwm-input-char-mode)))
              ;; (if (string= exwm-class-name "Termite")
              ;; (if (string= exwm-class-name "Gnome-terminal")
              ;;     (fhd/exwm-input-char-mode)
              ;;   (if (string= exwm-class-name "Termite")
              ;;       (fhd/exwm-input-char-mode)))
              (when-let ((target (cdr (assoc exwm-class-name exwm-workspace-window-assignments))))
                (progn
                  (exwm-workspace-move-window target)
                  (exwm-workspace-switch target)))))

  (defun fhd/launcher (command)
    "Lunch the app COMMAND."
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))


  (defun fhd/switch-to-last-window ()
    (interactive)
    (let ((win (get-mru-window t t t)))
      (unless win (error "Last window not found"))
      (let ((frame (window-frame win)))
        (raise-frame frame)
        (select-frame frame)
        (select-window win))))

  (exwm-input-set-key (kbd "s-.") #'fhd/copy-arrow-to-clipboard)
  (exwm-input-set-key (kbd "s-O") #'fhd/switch-to-last-window)
  (exwm-input-set-key (kbd "s-b") #'split-window-below)
  (exwm-input-set-key (kbd "s-c") #'list-processes)
  (exwm-input-set-key (kbd "s-d") #'fhd/launcher)
  (exwm-input-set-key (kbd "s-e") #'shell-pop)
  (exwm-input-set-key (kbd "s-f") #'exwm-floating-toggle-floating)
  (exwm-input-set-key (kbd "s-k") #'kill-this-buffer)
  (exwm-input-set-key (kbd "s-l") #'exwm-workspace-move-window)
  (exwm-input-set-key (kbd "s-o") #'other-window)
  (exwm-input-set-key (kbd "s-p") 'fhd/toggle-exwm-input-line-mode-passthrough)
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  (exwm-input-set-key (kbd "s-s") #'exwm-workspace-switch-to-buffer)
  (exwm-input-set-key (kbd "s-u") #'counsel-M-x)
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
  (exwm-input-set-key (kbd "<s-return>") #'fhd/open-in-terminal)

  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
  (exwm-input-set-key (kbd "s-i") #'fhd/exwm-input-toggle-mode)

  ;; (defun exwm-rename-buffer ()
  ;;   (interactive)
  ;;   ;; (concat exwm-class-name ":"
  ;;   ;;         (if (<= (length exwm-title) 40) exwm-title
  ;;   ;;           (concat (substring exwm-title 0 39) "...")))
  ;;   (exwm-workspace-rename-buffer exwm-class-name))


  ;; (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
  ;; (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)
  (defun fhd/run-initials ()
    "Run initialy needed apps like language layout indicator."
    (interactive)
    ;; (start-process-shell-command "changeLayout" nil "/home/farhad/bin/s &> /dev/null &")
    (start-process-shell-command "volumeicon" nil "volumeicon &> /dev/null &")
    (start-process-shell-command "cbatticon" nil "cbatticon &> /dev/null &")
    (start-process-shell-command "golden" nil "goldendict &> /dev/null &")
    (start-process-shell-command "gxkb" nil "gxkb &> /dev/null &"))

  (add-hook 'exwm-init-hook 'fhd/run-initials))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; exwm-config.el ends here
