(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-trigger-key "TAB")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(bookmark-default-file "~/emacs/bookmarks")
 '(column-number-mode t)
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("1bacdd5d24f187f273f488a23c977f26452dffbc82d4ac57250aa041f14159da" "959a77d21e6f15c5c63d360da73281fdc40db3e9f94e310fc1e8213f665d0278" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(default-input-method "farsi-isiri-9147")
 '(display-time-24hr-format nil)
 '(display-time-mode t)
 '(fci-rule-color "#515151")
 '(flycheck-eslintrc "/home/farhad/.eslintrc.json")
 '(ido-use-faces nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(js2-basic-offset 2)
 '(js2-strict-trailing-comma-warning nil)
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(next-screen-context-lines 3)
 '(org-drill-use-visible-cloze-face-p t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(register-preview-delay 0.5)
 '(save-place-file "~/emacs/saveplace.el")
 '(send-mail-function (quote mailclient-send-it))
 '(tags-table-list
   (quote
    ("/home/farhad/src/emacs24_5/emacs-24.5/lisp/TAGS" "/home/farhad/playground/TAGS")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil)
 '(x-select-enable-clipboard nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:weight normal))))
 '(compilation-mode-line-fail ((t (:inherit compilation-error :foreground "Red1" :weight normal))))
 '(css-property ((t (:inherit font-lock-variable-name-face :foreground "burlywood2"))))
 '(css-selector ((t (:inherit font-lock-function-name-face :foreground "CadetBlue1"))))
 '(cursor ((t (:background "DarkGoldenrod1"))))
 '(custom-variable-tag ((t (:foreground "light blue" :weight normal))))
 '(flx-highlight-face ((t (:inherit font-lock-variable-name-face :underline t :weight normal))))
 '(font-lock-comment-face ((t (:foreground "rosy brown" :slant italic :weight normal :family "SourceCodePro"))))
 '(font-lock-function-name-face ((t (:foreground "deep sky blue" :weight semi-light))))
 '(font-lock-keyword-face ((t (:background "gray25" :foreground "light gray" :weight semi-light :width normal))))
 '(font-lock-string-face ((t (:foreground "LightSkyBlue1"))))
 '(font-lock-variable-name-face ((t (:foreground "hot pink"))))
 '(js2-error ((t (:foreground "dark orange"))))
 '(js2-function-call ((t (:inherit default :foreground "burlywood3"))))
 '(js2-object-property ((t (:inherit default :foreground "hot pink"))))
 '(linum ((t (:background "#515151" :foreground "#99cc99" :slant normal :weight extra-light))))
 '(minibuffer-prompt ((t (:foreground "#ff79c6" :weight normal))))
 '(mode-line-buffer-id ((t (:weight normal))))
 '(mode-line-emphasis ((t nil)))
 '(org-done ((t (:foreground "orange" :box (:line-width 1 :color "#464752") :weight bold))))
 '(org-level-1 ((t (:foreground "light coral" :weight normal :height 1.1))))
 '(region ((t (:background "dim gray" :foreground "white" :inverse-video nil))))
 '(show-paren-match ((t (:background "#2d2d2d" :foreground "red" :underline t :weight bold)))))
