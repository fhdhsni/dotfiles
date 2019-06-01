(require 'package)
(require 'cl)
(require 'tls)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(package-initialize)

; (let ((secret.el "~/.secret.el.gpg"))
;   (when (file-exists-p secret.el)
;     (load secret.el)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)
(setq use-package-enable-imenu-support t)
(setq use-package-always-ensure t)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(use-package auto-compile
  :demand t
  :config (auto-compile-on-load-mode))

(use-package bind-key)
(use-package key-chord
  :disabled t
  :config
  (setq key-chord-two-keys-delay 0.2)
  (setq key-chord-one-key-delay 0.2)
  (defun fhd/key-chord-define (keymap keys command)
    "Define in KEYMAP, a key-chord of two keys in KEYS starting a COMMAND.
    \nKEYS can be a string or a vector of two elements. Currently only elements
    that corresponds to ascii codes in the range 32 to 126 can be used.
    \nCOMMAND can be an interactive function, a string, or nil.
    If COMMAND is nil, the key-chord is removed.

    MODIFICATION: Do not define the transposed key chord."
    (if (/= 2 (length keys))
        (error Key-chord keys must have two elements))
    ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
    (let ((key1 (logand 255 (aref keys 0)))
          (key2 (logand 255 (aref keys 1))))
      (define-key keymap (vector 'key-chord key1 key2) command)))
  (fset 'key-chord-define 'fhd/key-chord-define))

(use-package use-package-chords
  :disabled t
  :config
  (defmacro bind-chord (chord command &optional keymap)
    "Bind CHORD to COMMAND in KEYMAP (`global-map' if not passed)."
    (let ((key1 (logand 255 (aref chord 0)))
          (key2 (logand 255 (aref chord 1))))
      (if (eq key1 key2)
          `(bind-key (vector 'key-chord ,key1 ,key2) ,command ,keymap)
        `(progn
           (bind-key (vector 'key-chord ,key1 ,key2) ,command ,keymap)))))
  (key-chord-mode 1))

(setq user-full-name "Farhad Hasani"
      user-mail-address "farhad.hsni@gmail.com")

;; (defun fhd/quicksandify ()
;;   "Set font to a variable width (proportional) fonts in current buffer"
;;   (interactive)
;;   (setq buffer-face-mode-face '(:family "Quicksand-Bold"))
;;   (buffer-face-mode))

;; (add-hook 'text-mode-hook 'fhd/quicksandify)

(use-package visible-mark
  :config
  (defface visible-mark-one
    '((((type tty) (class mono)))
      (t (:underline (:color "dodgerblue" :style line))))
    "Example face which can be customized and added to subsequent face lists."
    :group 'visible-mark)

  (defface visible-mark-two
    '((((type tty) (class mono)))
      (t (:underline (:color "yellow" :style line))))
    "Example face which can be customized and added to subsequent face lists."
    :group 'visible-mark)

  (defface visible-mark-three
    '((((type tty) (class mono)))
      (t (:background "blue" :foreground "white")))
    "Example face which can be customized and added to subsequent face lists."
    :group 'visible-mark)

  (transient-mark-mode nil)
  (global-visible-mark-mode 1)
  (setq visible-mark-max 2)
  (setq visible-mark-faces `(visible-mark-one visible-mark-two visible-mark-three))

  (defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring)))))))

(use-package custom
  :ensure nil
  :demand t
  :init
  (setq custom-file "~/.emacs.d/custom.el"))

(use-package ibuffer
  :defer t
  :bind* (("C-x l" . ibuffer)))

(use-package hideshow
  :bind ("C-c h" . hs-toggle-hiding)
  :commands hs-toggle-hiding
  :defer t)

(use-package ispell
  :bind (("C-c s b" . ispell-buffer))
  :defer t)

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init (add-hook 'prog-mode-hook 'eldoc-mode)
  :commands eldoc-mode)

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package auto-compile :config (auto-compile-on-load-mode))

(fset 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(setq
 initial-scratch-message ""
 frame-title-format '("" "%b ")
 x-select-enable-clipboard t
 select-enable-clipboard t
 select-enable-primary t
 shell-file-name "zsh"
 shell-command-switch "-ic" ;don't know how but with these emacs become aware of my shell .aliases
 save-interprogram-paste-before-kill t
 browse-url-browser-function 'browse-url-default-browser
 cua-enable-cua-keys nil
 initial-major-mode 'text-mode
 x-use-underline-position-properties nil
 underline-minimum-offset 4
 load-prefer-newer t
 locale-coding-system 'utf-8
 create-lockfiles nil
 bookmark-default-file "~/emacs/bookmarks"
 default-input-method "farsi-isiri-9147"
 ;; display-time-24hr-format t
 backup-directory-alist `(("." . "~/emacs/backup_files/"))
 auto-save-file-name-transforms `((".*" "~/emacs/autoSaveFiles/" t))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 inhibit-startup-message t
 apropos-sort-by-scores t
 default-frame-alist '((font . "Fira Code Medium-14") (cursor-type . bar))
 scroll-margin 1
 scroll-conservatively 9999
 scroll-step 1
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 mouse-wheel-follow-mouse 't
 blink-cursor-interval 0.2
 save-place-file "~/emacs/saveplace.el"
 tab-stop-list (number-sequence 4 120 4)
 display-time-day-and-date t
 show-paren-style 'expression
 js-indent-level 2
 css-indent-offset 2)
(fringe-mode 0)
(setq indicate-empty-lines nil)
(blink-cursor-mode -1)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(electric-pair-mode t)
(show-paren-mode t)
(global-visual-line-mode -1)
(delete-selection-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(save-place-mode 1)
(global-hl-line-mode 1)
(display-time-mode 1)
(cua-mode)
(global-subword-mode)
(setq-default
 tab-width 4
 indent-tabs-mode nil)

(setq-default cursor-type '(bar . 2))

(defun fhd/on-frame-open (frame)
  (if (not (display-graphic-p frame))
      (set-face-background 'default "unspecified-bg" frame)))

(fhd/on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'fhd/on-frame-open)

(defun fhd/on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'fhd/on-after-init)

(use-package whitespace
  :defer t
  :bind* (("C-c w" . whitespace-cleanup)))

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<prior>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<next>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-M-<prior>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "S-M-<next>") 'enlarge-window)
(global-set-key (kbd "C-;") 'set-mark-command)
(global-set-key (kbd "C-?") 'dabbrev-expand)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "C-x |") 'fhd/toggle-window-split)
(global-set-key (kbd "M-F") 'forward-to-word)
(global-set-key (kbd "M-B") 'backward-to-word)
(global-set-key (kbd "M-u") 'universal-argument)
(global-set-key (kbd "M-U") 'upcase-dwim)
(global-set-key (kbd "C-h O") 'describe-face)
(global-set-key (kbd "C-x f") 'find-file-at-point)
(global-set-key (kbd "C-c C-c n") 'vc-find-conflicted-file)
(global-set-key (kbd "C-c C-.") 'override-global-mode)
(global-set-key (kbd "C-c C-k") 'kill-whole-line)
(global-set-key (kbd "C-x w") 'elfeed)
(global-set-key (kbd "M-j") 'counsel-M-x)
(global-set-key (kbd "M-*") 'quick-calc)
(global-set-key (kbd "M-n") 'mark-word)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'fhd/show-buffer-info)

(defun fhd/show-buffer-info ()
  (interactive)
  (save-excursion
    (let ((opoint (point)) beg end
          total before after)
      (forward-page)
      (beginning-of-line)
      (or (looking-at page-delimiter)
          (end-of-line))
      (setq end (point))
      (backward-page)
      (setq beg (point))
      (setq total (count-lines beg end)
            before (count-lines beg opoint)
            after (count-lines opoint end))

      (message  " %s: %d (%s + %d) %s"
                (propertize (buffer-name) 'face '(:foreground "#e97680"))
                total
                (propertize (number-to-string before) 'face '(:foreground "#e97680"))
                after
                major-mode))))

;; (defun fhd/delete-other-windows-and-hide-mode-line ()
;;   (interactive)
;;   (delete-other-windows)
;;   (fhd/nil-mode-line))
;; (defun fhd/split-below-show-mode-line ()
;;   (interactive)
;;   (split-window-below)
;;   (fhd/show-mode-line))
;; (global-set-key (kbd "C-x 1") 'fhd/delete-other-windows-and-hide-mode-line)
;; (global-set-key (kbd "C-x 2") 'fhd/split-below-show-mode-line)

;; (bind-key* (kbd "C-'") 'ivy-switch-buffer)

(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (unless (eq (window-start) (point-min))
    (scroll-down n)))

(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))

;; (bind-key* "C-P" 'scroll-down-in-place)
;; (bind-key* "C-N" 'scroll-up-in-place)

;; (global-set-key [remap dabbrev-expand] 'hippie-expand) ;use hippie-expand instead of dabbrev-expand

(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f
                        (scroll-bar-mode -1)
                        (toggle-frame-maximized)
                        (keyboard-translate ?\C-j ?\C-x)
                        (keyboard-translate ?\C-x ?\C-j))))

(keyboard-translate ?\C-j ?\C-x)
(keyboard-translate ?\C-x ?\C-j)

(use-package em-alias
  :defer t
  :ensure nil)
(use-package em-banner
  :defer t
  :ensure nil)
(use-package em-basic
  :defer t
  :ensure nil)
(use-package em-cmpl
  :defer t
  :ensure nil)
(use-package em-dirs
  :defer t
  :ensure nil)
(use-package em-glob
  :defer t
  :ensure nil)
(use-package em-hist
  :defer t
  :ensure nil)
(use-package em-ls
  :defer t
  :ensure nil)
(use-package em-prompt
  :defer t
  :ensure nil)
(use-package em-script
  :defer t
  :ensure nil)
(use-package em-term
  :defer t
  :ensure nil)
(use-package em-unix
  :defer t
  :ensure nil)

(require 'aweshell)
(use-package eshell
  :defer t
  :config
  ;; (add-hook 'eshell-preoutput-filter-functions
  ;;           'ansi-color-filter-apply)
  (defvar fhd/ansi-escape-re
    (rx (or ?\233 (and ?\e ?\[))
        (zero-or-more (char (?0 . ?\?)))
        (zero-or-more (char ?\s ?- ?\/))
        (char (?@ . ?~))))

  ;; (defun fhd/nuke-ansi-escapes (beg end)
  ;;   (save-excursion
  ;;     (goto-char beg)
  ;;     (while (re-search-forward fhd/ansi-escape-re end t)
  ;;       (replace-match ""))))
  ;; (defun fhd/eshell-nuke-ansi-escapes ()
  ;;   (fhd/nuke-ansi-escapes eshell-last-output-start eshell-last-output-end))
  ;; (add-hook 'eshell-output-filter-functions 'fhd/eshell-nuke-ansi-escapes t)
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize "\n[" 'face `(:foreground "green"))
           (propertize (concat (fhd/shortened-path (eshell/pwd) 40)) 'face `(:foreground "white"))
           (propertize "]\n" 'face `(:foreground "green"))
           (propertize "└─>" 'face `(:foreground "green"))
           (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "green"))))
        eshell-scroll-to-bottom-on-input t
        eshell-banner-message ""
        eshell-visual-commands
        '("elixir" "vim" "nps" "fzf" "tail" "htop" "ssh" "vi" "top" "less" "more" "jcal" "lynx" "ncftp")
        eshell-scroll-show-maximum-output nil
        eshell-history-size 10000)
  :demand t)

(use-package desktop
  :disabled t
  :config
  (progn
    (setq desktop-path '("~/emacs/"))
    (setq desktop-dirname "~/emacs/")
    (setq desktop-base-file-name "emacs-desktop")
    (setq desktop-globals-to-save
          (append '((extended-command-history . 50)
                    (file-name-history . 200)
                    (grep-history . 50)
                    (compile-history . 50)
                    (minibuffer-history . 100)
                    (query-replace-history . 100)
                    (read-expression-history . 100)
                    (regexp-history . 100)
                    (regexp-search-ring . 100)
                    (search-ring . 50)
                    (shell-command-history . 50)
                    tags-file-name
                    register-alist)))
    (desktop-save-mode 1)))

(use-package async
  :defer t
  :config
  (setq async-bytecomp-package-mode t))

(use-package define-word :defer t)
(use-package smartparens
  :demand t
  :init
  (require 'smartparens-config)
  :bind (("M-?" . sp-unwrap-sexp)
         ("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)
         ("C-}" . sp-beginning-of-next-sexp)
         ("C-{" . sp-beginning-of-previous-sexp)
         ("C-|" . sp-forward-slurp-sexp)
         ("C-c C-\"" . sp-up-sexp)
         ("C-:" . sp-forward-barf-sexp)))

(use-package multiple-cursors
  :defer t
  :init
  (setq mc/always-repeat-command t
        mc/always-run-for-all t)

  :bind (( "M-]" . mc/mark-next-like-this)
         ( "M-[" . mc/mark-previous-like-this)
         ( "M-N" . mc/skip-to-next-like-this)
         ( "M-P" . mc/skip-to-previous-like-this)
         ( "M-S-<mouse-1>" . mc/add-cursor-on-click)
         ( "M-A" . mc/mark-all-like-this)))

(use-package zop-to-char
  :defer t
  :bind (("C-c C-z" . zop-up-to-char)))

(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :init
  (progn
    (setq guide-key/guide-key-sequence
          '("C-c" "C-c t" "C-c s" "C-c h" "C-c p" "C-h" "C-," "C-, C-c" "C-x" "C-x a" "C-x 4" "C-x r" "C-x v" "M-s" "M-o" "M-g"))
    (guide-key-mode 1)))

(use-package guide-key-tip
  :after guide-key
  :init
  (setq guide-key-tip/enabled t))

(use-package emmet-mode
  :defer t
  :bind (("C->" . emmet-next-edit-point)
         ("C-<" . emmet-prev-edit-point))
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  :config
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (setq emmet-move-cursor-between-quotes t))

(use-package expand-region
  :demand t
  :bind* (("C-c C-'" . er/mark-inside-quotes)
          ("C-c C-9" . er/mark-inside-pairs)
          ("C-c f" . er/mark-defun)
          ("C-c C-0" . er/mark-outside-pairs)
          ("C-c j" . er/expand-region)))

(use-package diff-hl
  :defer t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package nginx-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

;; (use-package company-auctex
;;   :defer t
;;   :init
;;   (company-auctex-init))

(use-package restclient :defer t)
(use-package company-restclient :defer t)

(use-package plantuml-mode
  :defer t)

(use-package flycheck
  :bind (("C-M-m" . flycheck-mode)
         ("M-L" . flycheck-buffer))
  :config
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (setq flycheck-typescript-tslint-executable nil)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'html-tidy 'web-mode)))

(use-package flycheck-pos-tip
  :disabled t
  :after flycheck
  :defer t
  :init
  (flycheck-pos-tip-mode t))

(use-package jade-mode
  :defer t)

(use-package yasnippet
  :defer 15
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  (defun create-snippet (filename)
    "Creates snippet file in ~/.emacs.d/snippets/<mode-name> folder"
    (interactive "s")
    (let ((mode (symbol-name major-mode)))
      (find-file (format "~/.emacs.d/snippets/%s/%s" mode filename))
      (snippet-mode))))

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package elm-mode
  :defer t
  :init
  (setq elm-format-on-save t))

(use-package flycheck-elm
  :defer t)

;(use-package purescript-mode
;  :defer t
;  :ensure nil
;  :load-path "~/repos/purescript-mode")

; (require 'purescript-mode-autoloads)
; (add-to-list 'Info-default-directory-list "~/repos/purescript-mode/")
;
;(use-package psc-ide
;  :ensure t
;  :defer t
;  :init
;  (add-hook 'purescript-mode-hook
;            (lambda ()
;              (psc-ide-mode)
;              (company-mode)
;              (flycheck-mode)
;              (turn-on-purescript-indentation))))

(use-package haskell-mode
  :defer t)


;; (run-with-idle-timer 5 t (lambda ()
;;                              (cond
;;                               ((s-contains? "*Minibuf" (string-trim (buffer-name))) t)
;;                               (t (fhd/show-buffer-info)))))

(use-package minibuffer-line
  :disabled t
  :init
  (setq minibuffer-line-refresh-interval 0.1)
  (defun minibuffer-line--update ()
    (interactive)
    (with-current-buffer minibuffer-line--buffer
      (erase-buffer)
      (insert (format-mode-line minibuffer-line-format 'minibuffer-line))))
  (add-hook 'minibuffer-exit-hook 'minibuffer-line--update)
  (add-hook 'minibuffer-inactive-mode-hook 'minibuffer-line--update)
  (defface fhd/minibuffer-line-face
    '((t (:height .8 :foreground "white")))
    "face of minibuffer-line")

  (setq minibuffer-line-format
        '(""
          " "
          (:eval (propertize
                  (format "%s" (buffer-name))
                  'face
                  'fhd/minibuffer-line-face))
          " "
          ;; (:eval (propertize
          ;;         (format
          ;;          "%%l/%d:%%c "
          ;;          (line-number-at-pos
          ;;           (point-max)))
          ;;         'face
          ;;         'fhd/minibuffer-line-face))
          ))
  (minibuffer-line-mode))

(use-package elfeed
  :defer t
  :init
  (setq shr-width 80)
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "https://overreacted.io/rss.xml"
          "http://planet.emacsen.org/atom.xml")))

(use-package realgud
  :defer t)

(use-package multi-term
  :defer t
  :commands (multi-term)
  :config
  (setq multi-term-program "/usr/bin/zsh"))

(use-package magit
  :defer t
  :bind* (("C-c C-m" . magit-status))
  :config
  (diff-hl-mode)
  :init
  (setq magit-commit-arguments '("--no-verify"))
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-restore-window-configuration t) ; that's the default actually
  (setq magit-status-buffer-switch-function
        (lambda (buffer) ; there might already be an Emacs function which does this
          (pop-to-buffer buffer)
          (delete-other-windows)))
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  :commands (magit-status))

(use-package iy-go-to-char :defer t)
(use-package ix :defer t)
(use-package suggest :defer t)
(use-package elisp-refs :defer t)
(use-package s)
(use-package recentf
  :commands (recentf-open-files)
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 50))

(use-package scss-mode
  :defer t
  :mode "\\.scss\\'"
  :config (setq indent-tabs-mode nil tab-width 2))

(use-package md4rd
  :defer t)

(use-package json-mode
  :defer t
  :mode "\\.json\\'"
  :config
  (flycheck-mode)
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

(use-package avy
  :bind* (("C-c C-w" . avy-goto-line)
         ("C-c C-&" . avy-pop-mark)
         ("C-c C-l" . avy-goto-word-1)
         ("C-\"" . avy-goto-word-1)
         ("C-c C-r" . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-c C-'" . avy-isearch))
  :config
  (setq avy-timeout-seconds 0.4
        avy-keys-alist `((avy-goto-char-timer . (?a ?s ?d ?f ?g ?h ?j ?k ?l)))
        avy-style 'pre)
  (advice-add 'swiper :before 'avy-push-mark))

(use-package avy-zap
  :after avy
  :bind (("M-z" . zap-up-to-char)))

(use-package ace-window
  :custom
  (aw-keys '(?a ?n ?d ?f ?g ?h ?j ?k ?l)))

(use-package free-keys :commands (free-keys))

(use-package bm
  :after fhd-mode
  :config
  (define-key fhd-mode-map (kbd "C-, C-m") 'bm-toggle)
  (define-key fhd-mode-map (kbd "C-, C-p") 'bm-previous)
  (define-key fhd-mode-map (kbd "C-, C-z") 'zap-up-to-char)
  (define-key fhd-mode-map (kbd "C-, C-n") 'bm-next))

(use-package langtool
  :defer t
  :config
  (setq langtool-default-language "en-US")
  (setq langtool-java-classpath
        "/usr/share/languagetool:/usr/share/java/languagetool/*"))


(use-package web-mode
  :defer t
  :hook
  (web-mode . prettier-js-mode)
  (web-mode . tide-mode)
  (web-mode . flycheck-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (flymake-mode 0)
  (setq web-mode-enable-auto-quoting nil)
  :config
  (setq-default web-mode-enable-auto-quoting nil)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

  (flymake-mode 0)
  ;; (add-hook 'web-mode-hook #'lsp-vue-enable)
  ;; (add-hook 'web-mode-hook #'(lambda ()
  ;;                              (fhd/enable-minor-mode
  ;;                               '("\\.vue\\'" . prettier-js-mode))))
  ;; (add-hook 'web-mode-hook 'aggressive-indent-mode)
  ;; (add-hook 'web-mode-hook 'prettier-js-mode)
  ;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-enable-current-element-highlight t)
  (add-hook 'web-mode-hook 'emmet-mode)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 0)
  (setq web-mode-css-padding 0)
  (setq web-mode-block-padding 0)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-style-padding 0)
  (setq web-mode-markup-indent-offset 2))

(use-package jedi
  :disabled t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package elpy
  :disabled t
  :defer t
  :config
  (add-hook 'python-mode-hook 'elpy-mode))

(use-package git-gutter
  :disabled t
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode))

(use-package shell-pop
  :defer t
  :bind* (("C-c C-s" . shell-pop))
  :init
  (setq
   shell-pop-term-shell "eshell"
   shell-pop-shell-type '("eshell" "eshell" (lambda () (eshell)))
   shell-pop-universal-key "C-c C-s"
   shell-pop-window-size 50
   shell-pop-window-position "right"))

(use-package sdcv :bind (("C-c C-#" . sdcv-search-pointer+)))

(use-package google-translate
  :defer t
  :bind ("C-C G" . google-translate-smooth-translate)
  :init
  (setq-default google-translate-translation-directions-alist
                '(("fr" . "en") ("en" . "fr"))
                google-translate-show-phonetic t))

(use-package know-your-http-well
  :commands (http-header http-method http-relation http-status-code media-types))

(use-package paradox
  :disabled t
  :commands paradox-list-packages
  :config
  (setq paradox-github-token t
        paradox-automatically-star nil
        paradox-execute-asynchronously t))

(use-package quickrun
  :bind ("C-c x" . quickrun))

(use-package yaml-mode :defer t)

(use-package visual-regexp :defer t)

(use-package move-text
  :defer t
  :init
  (move-text-default-bindings))

(use-package go-mode :defer t)

(use-package find-file-in-project
  :defer t
  :init
  (setq ffip-use-rust-fd t)
  :pin melpa)

(use-package ruby-end
  :defer t)

(use-package elixir-mode
  :diminish elixir-mode
  :hook
  (elixir-mode . lsp)
  (elixir-mode . smartparens-mode)
  (elixir-mode . show-smartparens-mode)
  :init
  (add-to-list 'exec-path "/home/farhad/repos/elixir-ls/release")
  (fset 'fhd/unshort-elixir-def
        (lambda (&optional arg)
          "Keyboard macro."
          (interactive "p")
          (kmacro-exec-ring-item (quote ([19 100 111 58 13 18 44 return 4 19 58 4 13 5 13 101 110 100 1 9] 0 "%d")) arg)))

  (fset 'fhd/make-def
        (lambda (&optional arg)
          "Keyboard macro."
          (interactive "p")
          (kmacro-exec-ring-item (quote ("\355def  do	" 0 "%d")) arg)))

  :config
  ;; (add-to-list 'elixir-mode-hook
  ;;              (defun auto-activate-ruby-end-mode-for-elixir-mode ()
  ;;                (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
  ;;                     "\\(?:^\\|\\s-+\\)\\(?:do\\)")
  ;;                (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
  ;;                (ruby-end-mode +1)))
  (add-hook 'elixir-mode-hook 'flycheck-mode)
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

  :bind (("C-c a u" . fhd/unshort-elixir-def)
         ("C-c a d" . fhd/make-def))
  :defer t)

(use-package alchemist
  :disabled t
  :diminish alchemist-mode
  :after elixir-mode
  :defer t
  :init
  (alchemist-mode t))


;; (use-package eglot
;;   :bind* (("C-, d" . eglot-help-at-point))
;;   :config
;;   (add-to-list 'eglot-server-programs '(web-mode . ("javascript-typescript-stdio" "")))
;;   (add-to-list 'eglot-server-programs '(reason-mode . ("reason-language-server.exe" ""))))

;; (require 'ivy-erlang-complete)
(use-package erlang
  :defer t
  :bind (:map erlang-mode-map
              ("C-`" . ivy-erlang-complete)
              ("C-C a" . fhd/capitalize-previous-word)
              ("C-c C-k" . fhd/compile-and-display-erlang))
  :config
  (setq load-path (cons  "/usr/lib/erlang/lib/tools-3.0.1/emacs" load-path))
  (setq erlang-root-dir "/usr/lib/erlang")
  (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
  (setq erlang-man-root-dir "/usr/lib/erlang/man")
  (defun fhd/compile-and-display-erlang ()
    (interactive)
    (erlang-compile)
    (erlang-compile-display))

  (add-hook 'erlang-shell-mode-hook #'company-mode)
  ;; (add-hook 'erlang-mode-hook #'ivy-erlang-complete-init)
  ;; (add-hook 'after-save-hook #'ivy-erlang-complete-reparse)
  ;; (add-hook 'erlang-mode-hook #'company-erlang-init)
  (add-hook 'erlang-mode-hook
            (lambda ()
              (flycheck-mode)
              (company-mode)
              (smartparens-mode)
              (erlang-edoc-mode))))

(use-package w3m :disabled t :defer t)

(use-package writeroom-mode
  :defer t)

(use-package erc
  :defer t
  :config
  (progn
    (add-to-list 'erc-modules 'notifications)
    (setq
     erc-autojoin-channels-alist '(("freenode.net" . ("#emacs")))
     ;; erc-track-exclude-types '("JOIN" "PART" "NICK" "MODE" "QUIT")
     erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                               "324" "329" "332" "333" "353" "477")
     erc-hide-list '("JOIN" "PART" "QUIT")
     erc-nick "fhdhsni"
     erc-port 7070
     erc-password fhd/password
     erc-server "chat.freenode.net")))

(use-package unbound
  :defer t)

(use-package mmm-mode :defer t)

(use-package dumb-jump
  :defer t
  :bind (("C-c g" . dumb-jump-go))
  :config
  (setq dumb-jump-selector 'ivy))


(use-package helpful
  :config
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

(use-package fold-this
  :bind (:map fold-this-keymap
              ("c-c f" . fold-this))
  :defer t)

(use-package easy-kill
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill))


(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package sicp
  :defer t)

(use-package helpers
  :demand t
  :ensure nil
  :bind* (
          ;; ([remap kill-region] . cut-line-or-region)
          ;; ([remap kill-ring-save] . copy-line-or-region)
          ("C-c C-n" . fhd/toggle-comment-on-line)
          ("C-c i" . fhd/open-init)
          ("C-c C-j" . fhd/jump-to-mark)
          ("C-)" . fhd/region-to-bottom)
          ("C-(" . fhd/region-to-top)
          ("C-c C-=" . fhd/surround-the-word-under-point-with-equal)
          ("C-c C--" . fhd/surround-the-word-under-point-with-equal)
          ("M-s y" . fhd/copy-word-under-point)
          ("C-c t" . fhd/open-in-gui-filemanager)
          ("C-c C-d" . fhd/duplicate-current-line-or-region)
          ("C-x O" . fhd/switch-to-last-window)))

(use-package projectile
  :bind ("C-, C-p C-s" . projectile-run-eshell)
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package ag
  :defer t)

(use-package treemacs
  :defer t
  :bind ("<f8>" . treemacs)
  :init
  (with-eval-after-load "treemacs"
    (setq
     treemacs-icon-fallback-text (propertize "> " 'face 'font-lock-keyword-face)
     treemacs-icon-tag-node-open-png   (propertize "− " 'face 'font-lock-keyword-face)
     treemacs-icon-tag-node-closed-png (propertize "+ " 'face 'font-lock-keyword-face)
     treemacs-icon-tag-leaf-png        (propertize "> " 'face 'font-lock-keyword-face))
    (setq treemacs-icon-tag-leaf-text (propertize "> " 'face 'font-lock-constant-face)))
  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-file-event-delay           10000
          treemacs-width                      30
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             t
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              t
          treemacs-indentation-string         " "
          treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)))

(use-package treemacs-projectile
  :defer t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

(use-package indent-guide
  :init
  (indent-guide-global-mode)
  (setq indent-guide-char "|")
  (setq indent-guide-delay 8))

(use-package highlight-symbol
  :init
  (highlight-symbol-mode t)
  (highlight-symbol-nav-mode t))

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :defer t
  :mode "\\.rs\\'"
  :bind ("C-c c-r" . cargo-process--command-run))

(use-package racer
  :mode "\\.rs\\'"
  :after rust-mode
  :config
  (add-hook 'racer-mode-hook #'company-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package flycheck-rust :defer t)

(use-package autoinsert
  :init
  (add-hook 'find-file-hooks 'auto-insert)
  (setq auto-insert-alist
        '(("\\.scm" .
           (insert "#!/bin/sh\n#| -*- scheme -*-\nexec csi -s $0 \"$@\"\n|#\n")))))

(use-package geiser
  :defer t
  :init
  (with-eval-after-load "geiser-mode"
    ;; (setq geiser-active-implementations '(guile))
    (setq geiser-autodoc-delay 5)
    (define-key geiser-mode-map (kbd "C-.") nil)))

(use-package lispy
  :hook ((scheme-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode))
  :init
  (with-eval-after-load "lispy"
    ;; (setq prettify-symbols-alist '(("lambda" . ?λ)))
    ;; (prettify-symbols-mode)
    (setq lispy-no-permanent-semantic t)))

(use-package aggressive-indent
  :disabled t
  :defer t
  :diminish aggressive-indent-mode
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode)

  (setq aggressive-indent-excluded-modes
        '(bibtex-mode
         web-mode
         elixir-mode
         cider-repl-mode
         coffee-mode
         comint-mode
         conf-mode
         Custom-mode
         diff-mode
         doc-view-mode
         dos-mode
         erc-mode
         feature-mode
         fortran-mode
         f90-mode
         jabber-chat-mode
         haml-mode
         haskell-mode
         haskell-interactive-mode
         image-mode
         inf-ruby-mode
         makefile-mode
         makefile-gmake-mode
         minibuffer-inactive-mode
         netcmd-mode
         python-mode
         sass-mode
         scala-mode
         slim-mode
         special-mode
         shell-mode
         snippet-mode
         eshell-mode
         tabulated-list-mode
         term-mode
         TeX-output-mode
         text-mode
         yaml-mode
         typescript-mode)))

(use-package zeal-at-point
  :bind ("C-c z" . zeal-at-point))

(use-package rainbow-delimiters
  :hook (lispy-mode . rainbow-delimiters-mode))

(use-package dimmer
  :init
  (setq dimmer-fraction 0.31)
  (dimmer-mode))

(use-package volatile-highlights
  :init (volatile-highlights-mode t))

(use-package toml-mode
  :defer t)

(use-package phi-search)
;; (global-set-key (kbd "C-s") 'phi-search)
;; (global-set-key (kbd "C-r") 'phi-search-backward)
(use-package reason-mode
  :hook
  (before-save . refmt-before-save)
  (reason-mode . lsp)
  :config
  ;;----------------------------------------------------------------------------
  (fset 'fhd/tore
        (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("	<sr	''" 0 "%d")) arg)))

  (defun shell-cmd (cmd)
    "Returns the stdout output of a shell command or nil if the command returned
   an error"
    (car (ignore-errors (apply 'process-lines (split-string cmd)))))

  (defun reason-cmd-where (cmd)
    (let ((where (shell-cmd cmd)))
      (if (not (string-equal "unknown flag ----where" where))
          where)))

  (let* ((refmt-bin (or (reason-cmd-where "refmt ----where")
                        (shell-cmd "which refmt")
                        (shell-cmd "which bsrefmt")))
         (merlin-bin (or (reason-cmd-where "ocamlmerlin ----where")
                         (shell-cmd "which ocamlmerlin")))
         (merlin-base-dir (when merlin-bin
                            (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
    ;; Add merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
    (when merlin-bin
      (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
      (setq merlin-command merlin-bin))

    (when refmt-bin
      (setq refmt-command refmt-bin))))


(use-package prettier-js
  :bind (:map fhd-mode-map
              ("C-c C-p" . prettier-js))
  :init
  (setq prettier-js-args '(
                           "--trailing-comma" "all"
                           "--bracket-spacing" "true"
                           "--single-quote" "false"
                           "--print-width" "80"
                           "--tab-width" "2"
                           ))
  (add-hook 'typescript-mode-hook 'prettier-js-mode))
;; (setq prettier-js-args '("--parser" "typescript" "--trailing-comma" "all" "--print-width" "80" "--bracket-spacing" "true"))


(use-package fhd-mode
  :demand t
  :ensure nil
  :init
  (define-prefix-command 'fhd-mode-map)
  (defun fhd/jump ()
    (interactive)
    (set-mark (point))
    (cua-set-mark 4))
  :bind (:map fhd-mode-map
              ("C-, C-j" . join-line)
              ("C-, n" . flymake-goto-next-error)
              ("C-, C-s" . eshell)
              ("C-, p" . flymake-goto-prev-error)
              ("C-, C-t" . toggle-truncate-lines)))

(use-package dockerfile-mode
  :ensure t)


(use-package deadgrep
  :config
  (global-set-key (kbd "<f5>") #'deadgrep)
  (global-set-key (kbd "<XF86Open>") #'deadgrep))

(with-eval-after-load "cus-edit"
  (set-face-attribute  'custom-variable-tag nil :weight 'normal)
  (set-face-attribute 'custom-face-tag nil :weight 'normal))

(set-face-attribute 'bold nil :weight 'normal)

(defalias 'rb 'revert-buffer)
(defalias 'x 'js2-jsx-mode)
(defalias 'oil 'org-insert-link)
(defalias 'sl 'sort-lines)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'sp 'shell-pop)
(defalias 'wm 'writeroom-mode)
(defalias 'el 'emacs-lisp-mode)

;; (defun fhd/foobar ()
;;   (interactive)
;;   (if current-prefix-arg
;;       (message "hi")
;;     (message "bye")))

;;  (defun fhd/foobar ()
;;    (interactive)
;;    (if (interactive "P")
;;        (message "hi")
;;      (message "bye")))

;; (defun fhd/jar (x) (interactive "P")
;;        (message (if x "hi" "bye")))

;; (defun test (p)                         ;
;;   (interactive "p")
;;   (message "Interactive: %s" p))

;; (defun test (p)
;;   (interactive "p")
;;   (if (= p 4)
;;       (message "hi")
;;     (message "bye")))



(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" nil))
    ad-do-it
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" agent))))


(defun fhd/load-user-file (file)
  "Load a FILE in current user's configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))

;; (fhd/load-user-file "lisp/modeline-config.el")
;; (fhd/load-user-file "lisp/java.el")
(fhd/load-user-file "lisp/lsp.el")
(fhd/load-user-file "lisp/awesome-tray.el")
(fhd/load-user-file "lisp/orgmode-config.el")
(fhd/load-user-file "lisp/javascript-config.el")
(fhd/load-user-file "lisp/ivy-config.el")
;; (fhd/load-user-file "lisp/helm-config.el")
;; (fhd/load-user-file "lisp/minibuffer-config.el")
;; (fhd/load-user-file "lisp/proced-config.el")
(fhd/load-user-file "lisp/typescript-config.el")
(fhd/load-user-file "lisp/moe-dark-theme.el")
(custom-set-faces
 '(cursor ((t (:background "yellow"))))
 '(visible-mark-active ((t nil)))
 '(font-lock-comment-face ((t (:foreground "gray65" :slant italic :height 1.0))))
 '(mode-line ((t (:box nil :background "grey80" :height .1))))
 '(mode-line-inactive ((t (:box nil :background "grey30" :height .1))))
 '(mmm-default-submode-face ((t nil)))
 '(treemacs-root-face ((t (:inherit font-lock-constant-face :height 1.0))))
 '(mode-line-buffer-id ((t (:weight light :foreground "darkmagenta")))))
;; (setq-default mode-line-format '("hi"))
(setq-default mode-line-format nil)
(fhd/load-user-file "lisp/dired-config.el")
(fhd/load-user-file "lisp/company-config.el")
(fhd/load-user-file "lisp/hydra-config.el")
;; (fhd/load-user-file "lisp/exwm-config.el")
;; (fhd/load-user-file "lisp/emacs-auctex-config.el")
;; (fhd/load-user-file "lisp/php-config.el")
;; (fhd/load-user-file "lisp/haproxy.el")

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'isearch-mode-end-hook
          #'endless/goto-match-beginning)
(defun endless/goto-match-beginning ()
  "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
  (when (and isearch-forward
             (number-or-marker-p isearch-other-end)
             (not mark-active)
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

;; (progn
;;   ;; overlay an arrow where the mark is
;;   (defvar fringemark-overlay-arrow-position)
;;   (make-variable-buffer-local 'fringemark-overlay-arrow-position)
;;   ;; (delq 'fringemark-overlay-arrow-position overlay-arrow-variable-list)
;;   (add-to-list 'overlay-arrow-variable-list  'fringemark-overlay-arrow-position)
;;   (defun fringemark-mark-hook ()
;;     ;; (make-local-variable 'fringemark-overlay-arrow-position)
;;     (unless (or (minibufferp (current-buffer)) (not (mark)))
;;       (set
;;        'fringemark-overlay-arrow-position
;;        (save-excursion
;;          (goto-char (mark))
;;          (forward-line 0)
;;          (point-marker)))))
;;   (add-hook 'post-command-hook 'fringemark-mark-hook)

;;   ;; make the mark fringe bitmap look cool dude
;;   (define-fringe-bitmap 'fringemark-hollow-right-arrow [128 192 96 48 24 48 96 192 128] 9 8 'center)
;;   (put 'fringemark-overlay-arrow-position 'overlay-arrow-bitmap 'fringemark-hollow-right-arrow))

;; (provide 'fringemark)


(defun std::pacman-pkg-info ()
  (interactive)
  (let* ((completions (->> "pacman -Q"
                           (shell-command-to-string)
                           (s-trim)
                           (s-lines)
                           (--map (car (s-split " " it :no-nulls)))))
         (name (completing-read "Package: " completions)))
    (switch-to-buffer (get-buffer-create "*Package Info*"))
    (erase-buffer)
    (-> (format "pacman -Qi %s" name)
        (shell-command-to-string)
        (s-trim)
        (insert))
    (goto-char 0)
    (conf-mode)))

;; (require 'vue-mode)
;; (require 'lsp-mode)
;; (setq lsp-auto-guess-root t)  ;; if you have projectile ...

;; (defun vuejs-custom ()
;;   (lsp)
;;   (push 'company-lsp company-backends)
;;   (flycheck-mode t)
;;   (prettier-js-mode)
;;   (company-mode))

;; (add-hook 'vue-mode-hook 'vuejs-custom)

;; (add-hook 'minibuffer-setup-hook
;;           (lambda ()
;;             (make-local-variable 'face-remapping-alist)
;;             (add-to-list 'face-remapping-alist '(default (:background "black")))))

(use-package    feebleline
  :ensure       t
  :custom       (feebleline-show-git-branch             nil)
                (feebleline-show-dir                    t)
                (feebleline-show-time                   nil)
                (feebleline-show-previous-buffer        t)
  :config       (feebleline-mode 1))

;; (require 'awesome-tray)
;; (setq awesome-tray-active-modules '("buffer-name" "location"))
;; (setq awesome-tray-mode-line-active-color "darkcyan")
;; ;; ;; (setq awesome-tray-mode t)
;; (custom-set-faces
;;  '(awesome-tray-module-buffer-name-face ((t (:foreground "#ff9500"))))
;;  '(awesome-tray-module-date-face ((t (:foreground "#8e8e93"))))
;;  '(awesome-tray-module-git-face ((t (:foreground "#ff2d55"))))
;;  '(awesome-tray-module-last-command-face ((t (:foreground "#007aff"))))
;;  '(awesome-tray-module-location-face ((t (:foreground "#ff9500"))))
;;  '(awesome-tray-module-mode-name-face ((t (:foreground "green3"))))
;;  '(awesome-tray-module-parent-dir-face ((t (:foreground "#9DED4D"))))
;;  '(awesome-tray-module-rvm-face ((t (:foreground "#333fff")))))
;; (awesome-tray-mode 1)

(set-face-attribute 'default (selected-frame) :height 130)
