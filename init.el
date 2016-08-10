(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(setq user-full-name "Farhad Hassani"
      user-mail-address "farhad.hsni@gmail.com")

(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))

(use-package use-package-chords
  :ensure t
  ;; :load-path "~/git/usePackageChords/"
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

(fset 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq load-prefer-newer t)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq backup-directory-alist `(("." . "~/emacs/backup_files/")))
(setq save-place-file "~/emacs/saveplace.el")
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default cursor-type 'bar)
(set-cursor-color "#0f0")
(global-hl-line-mode t)
;; (set-face-background 'hl-line "#0f0f0f")
(set-face-background 'hl-line "#000")
(set-face-foreground 'highlight nil)
(setq apropos-sort-by-scores t)
;; (set-frame-font "SourceCodePro-13")
(setq default-frame-alist '((font . "Operator Mono Book-11")))
(setq scroll-margin 1
      scroll-conservatively 9999
      scroll-step 1)
(electric-pair-mode t)
(show-paren-mode t)
(global-visual-line-mode t) ;don't break lines in the middle of words
(delete-selection-mode t)
(menu-bar-mode -1)
(show-paren-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-indicate-empty-lines t)

;; to get rid of that ugly blue background color in terminal
(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
      (set-face-background 'default "unspecified-bg" frame)))
(on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'on-frame-open)

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)
;; END

(fset 'node2
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 104 134217852 110 111 100 101 return 21 67108923 21 67108923] 0 "%d")) arg)))

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "<f8>") 'node2) ;
(global-set-key (kbd "<f5>") 'save-buffer)
(global-set-key (kbd "M-p") 'other-window)
(global-set-key (kbd "C-;") 'set-mark-command)
;; (global-unset-key (kbd "C-z"))          ;c-z do nothing
(global-set-key [remap dabbrev-expand] 'hippie-expand) ;use hippie-expand instead of dabbrev-expand

(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f
                        (scroll-bar-mode -1)
                        (keyboard-translate ?\C-j ?\C-x)
                        (keyboard-translate ?\C-x ?\C-j))))

(keyboard-translate ?\C-j ?\C-x)
(keyboard-translate ?\C-x ?\C-j)

(use-package smartparens
  :ensure t
  :commands (sp-unwrap-sexp)
  :bind (("M-?" . sp-unwrap-sexp)
         ("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)))

(use-package ace-jump-mode
  :disabled t
  :ensure t
  :defer t
  :chords ((";r" . ace-jump-mode)))

(use-package aggressive-indent
  :ensure t
  :defer t
  :config (global-aggressive-indent-mode t)
  :diminish aggressive-indent-mode)

(use-package powerline
  :disabled t
  :ensure nil
  :load-path "~/git/powerline/"
  :pin manual
  :config (powerline-default-theme)
  (setq powerline-default-separator (quote bar))
  (setq powerline-gui-use-vcs-glyph t))

(use-package neotree
  :ensure nil
  :load-path "~/git/neotree/"
  :pin manual
  :commands (neotree-toggle)
  :bind ("C-#" . neotree-toggle)
  :config
  (setq neo-smart-open t)
  (setq neo-theme (quote arrow)))


(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (( "M-]" . mc/mark-next-like-this)
         ( "M-[" . mc/mark-previous-like-this)
         ( "M-N" . mc/skip-to-next-like-this)
         ( "M-P" . mc/skip-to-previous-like-this)
         ( "M-A" . mc/mark-all-like-this)))

(use-package zop-to-char
  :ensure t
  :defer t
  :bind (("C-z" . zop-up-to-char)))



(use-package helm-descbinds
  :ensure t
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package helm-swoop
  :ensure t
  :bind
  (("M-s s" . helm-swoop)
   ("M-s M-s" . helm-swoop))
  :config
  (progn
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)))

(use-package guide-key
  :defer t 
  :ensure t
  :diminish guide-key-mode
  :init
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c h" "C-x a"))
    (guide-key-mode 1)))

(use-package emmet-mode
  :ensure t
  :defer t
  :bind (("C->" . emmet-next-edit-point)
         ("C->" . emmet-prev-edit-point))
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  :config
  (setq emmet-move-cursor-between-quotes t))

(use-package smex
  :disabled t
  :ensure t
  :bind (("C-x C-u" . smex))
  :config (smex-initialize))

(use-package expand-region
  :ensure t
  :defer t
  :chords ((";e" . er/expand-region)
           (";i" . er/mark-js-if)
           (";'" . er/mark-inside-quotes)
           (";p" . er/mark-inside-pairs))
  :bind (("M-F" . er/mark-js-function)))

(use-package flycheck
  :ensure t
  :defer t
  :bind (("C-M-m" . flycheck-mode)
         ("M-L" . flycheck-error-list))
  :config
   (setq flycheck-eslintrc "/home/farhad/.eslintrc.json"))

(use-package auto-complete
  :ensure t
  :init
  (auto-complete-mode t)
  :config
  (ac-config-default)
  (define-key ac-completing-map "\r" nil) ;don't use enter plz
  (setq ac-auto-show-menu 0.3)
   (setq ac-trigger-key "TAB"))

(use-package cheatsheet
  :ensure t
  :defer t
  :chords ((";j" . cheatsheet-show))
  :commands (cheatsheet-show)
  :config
  (cheatsheet-add :group 'General
                  :key "M-S-SPC"
                  :description "just-one-space")
  (cheatsheet-add :group 'dired-x
                  :key "C-x C-j"
                  :description "Jump to dir of current buffer")
  (cheatsheet-add :group 'dired-x
                  :key "*."
                  :description "mark file by extension. e.g *.js")
  (cheatsheet-add :group 'projectile
                  :key "f / 4 f"
                  :description "Display a list of all files in the project. With a prefix argument it will clear the cache first.")
  (cheatsheet-add :group 'projectile
                  :key "4 g"
                  :description "Jump to a project's file based on context at point and show it in another window.")
  (cheatsheet-add :group 'projectile
                  :key "d / 4 d"
                  :description "Switch to a project directory [and show it in another window].")
  (cheatsheet-add :group 'projectile
                  :key "D"
                  :description "Open the root of project in Dired")
  (cheatsheet-add :group 'projectile
                  :key "P"
                  :description "Run a test command"))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode t)
  :bind (("C-." . create-snippet))
  :config
  (defun create-snippet (filename)
  "Creates snippet file in ~/.emacs.d/snippets/<mode-name> folder"
  (interactive "s")
  (let ((mode (symbol-name major-mode)))
    (find-file (format "~/.emacs.d/snippets/%s/%s" mode filename))
    (snippet-mode))))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package ranger
  :ensure t
  :defer t
  :commands (ranger)
  :chords ((";q" . ranger))
  :config
  (setq ranger-dont-show-binary t)
  (setq ranger-max-preview-size 10)
  (setq ranger-excluded-extensions '("mkv" "mp4" "mp3" "iso" "mp4"))
  (setq ranger-show-dotfiles -1)
  (setq ranger-override-dired t))

(use-package projectile
  :ensure t
  :defer t
  :commands R
  :config
  (define-key projectile-mode-map (kbd "C-x a") 'projectile-command-map))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package nyan-mode
  :ensure t
  :init
  (nyan-mode t))

(use-package multi-term
  :ensure t
  :commands (multi-term)
  :config
  (setq multi-term-program "/usr/bin/fish"))

(use-package magit
  :ensure t
  :defer t
  :commands (magit-status)
  :config
  (setq magit-diff-use-overlays nil)
  :chords ((";g" . magit-status)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2)
  (setq js2-strict-trailing-comma-warning nil)
  (defun jquery-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("jQuery") ) )

  (add-hook 'js2-mode-hook 'jquery-doc)
  (setq helm-dash-common-docsets '("JavaScript"))
  (add-hook 'js2-mode-hook 'hs-minor-mode)

  ;; tern stuff
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'load-path "~/.node_modules_global/lib/node_modules/tern/emacs")
  (autoload 'tern-mode "tern.el" nil t)
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))

  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup)))
  (use-package context-coloring
    :ensure t
    ;; :mode ("\\.js\\'" . context-coloring-mode)
    :config
    ;; JavaScript:
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'context-coloring-mode)

    ;; eval-expression:
    (add-hook 'eval-expression-minibuffer-setup-hook #'context-coloring-mode)
    (add-hook 'minibuffer-setup-hook #'context-coloring-mode)

    (use-package tern-context-coloring
      :ensure t
      :config
      (eval-after-load 'context-coloring
        '(tern-context-coloring-setup))
      (add-hook 'js2-mode-hook (lambda ()
                                 (unless (eq major-mode 'json-mode)
                                   (tern-mode)
                                   (context-coloring-mode)))))))

(use-package key-chord
  :ensure 
  :demand t
  :config
  (defun my/key-chord-define (keymap keys command)
    "Define in KEYMAP, a key-chord of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

MODIFICATION: Do not define the transposed key chord.
"
    (if (/= 2 (length keys))
        (error "Key-chord keys must have two elements"))
    ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
    (let ((key1 (logand 255 (aref keys 0)))
          (key2 (logand 255 (aref keys 1))))
      (define-key keymap (vector 'key-chord key1 key2) command)))
  (fset 'key-chord-define 'my/key-chord-define)
  (key-chord-mode 1)

  (setq key-chord-two-keys-delay 0.2)
  (setq key-chord-one-key-delay 0.3)
  (key-chord-define-global ";a" 'mark-whole-buffer)
  (key-chord-define-global ";h" 'hs-toggle-hiding))



(use-package iy-go-to-char
  :ensure t
  :chords ((";f" . iy-go-to-char)
           (";t" . iy-go-up-to-char)))

(use-package helm-dash
  :ensure t
  :commands (helm-dash)
  :chords ((";s" . helm-dash)))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-listing-switches "-Ahl --time-style long-iso")
  (add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))
  ;; dired hide details <start>
  (defun xah-dired-mode-setup ()
    "to be run as hook for `dired-mode'."
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'xah-dired-mode-setup)
  ;; dired hide details <end>
  (setq dired-recursive-deletes (quote top))
  (define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda ()
                                         (interactive)
                                         (find-alternate-file "..")))
  (put 'dired-find-alternate-file 'disabled nil))

(use-package recentf
  :commands (recentf-open-files)
  :bind (("<f6>" . recentf-open-files))
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25))

(use-package ido
  :disabled t
  :ensure t
  :demand t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-use-faces nil)
  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode))
  (setq ido-use-faces nil)
  (setq ido-enable-flex-matching t))

(use-package scss-mode
    ;; (autoload 'scss-mode "scss-mode")
  ;; (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))  
  :ensure t
  :mode "\\.scss\\'"
  :config
  (setq exec-path (cons (expand-file-name "~/.gem/ruby/2.3.0/bin") exec-path)))

(use-package org-mode
  :ensure nil
  :load-path "~/git/org-mode/lisp"
  :mode "\\.org\\'"
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :config
  (setq org-modules
    (quote
     (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m)))
  (setq org-log-done t)
  (setq org-agenda-files (list "~/orgmode/agenda.org"))
  (setq org-todo-keywords
        '((sequence "TODO" "CANCELED" "IN-PROGRESS" "WAITING" "DONE")))
  (add-hook 'org-mode-hook (lambda()
                             (set (make-local-variable 'electric-indent-functions)
                                  (list (lambda(org) 'no-indent)))))
  (setq org-src-fontify-natively t))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package hydra
  :ensure t
  :defer t
  :config
  (defhydra hydra-window (global-map "C-,")
    "
Movement^^        ^Split^         ^Switch^      ^Resize^
----------------------------------------------------------------
_h_ ←         _v_ertical      _b_uffer        _q_ X←
_j_ ↓         _x_ horizontal  _f_ind files    _w_ X↓
_k_ ↑         _z_ undo        _a_ce 1     _e_ X↑
_l_ →         _Z_ reset       _s_wap      _r_ X→
_F_ollow        _D_lt Other     _S_ave      max_i_mize
_SPC_ cancel    _o_nly this     _d_elete    
"
    ("h" windmove-left )
    ("j" windmove-down )
    ("k" windmove-up )
    ("l" windmove-right )
    ("q" hydra-move-splitter-left)
    ("w" hydra-move-splitter-down)
    ("e" hydra-move-splitter-up)
    ("r" hydra-move-splitter-right)
    ("b" helm-mini)
    ("f" helm-find-files)
    ("F" follow-mode)
    ("a" (lambda ()
           (interactive)
           (ace-window 1)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body))
     )
    ("v" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right))
     )
    ("x" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down))
     )
    ("s" (lambda ()
           (interactive)
           (ace-window 4)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
    ("S" save-buffer)
    ("d" delete-window)
    ("D" (lambda ()
           (interactive)
           (ace-window 16)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body))
     )
    ("o" delete-other-windows)
    ("i" ace-maximize-window)
    ("z" (progn
           (winner-undo)
           (setq this-command 'winner-undo))
     )
    ("Z" winner-redo)
    ("SPC" nil)
    ))

(use-package corral
  :ensure t
  :config
  (defhydra hydra-corral (:columns 4)
    "Corral"
    ("(" corral-parentheses-backward "Back")
    (")" corral-parentheses-forward "Forward")
    ("[" corral-brackets-backward "Back")
    ("]" corral-brackets-forward "Forward")
    ("{" corral-braces-backward "Back")
    ("}" corral-braces-forward "Forward")
    ("." hydra-repeat "Repeat"))
  (global-set-key (kbd "C-c c") #'hydra-corral/body))

(use-package avy
  :ensure t
  :defer t
  :chords ((";r" . avy-goto-char-2))
  :config
  ;; (defhydra hydra-avy (global-map "M-g" :color blue)
  ;;   "avy-goto"
  ;;   ("c" avy-goto-char "char")
  ;;   ("w" avy-goto-word-1 "word")
  ;;   ("s" avy-goto-subword-1 "subword"))
  (global-set-key (kbd "M-g g") #'avy-goto-line))

(use-package form-feed
  :defer t
  :ensure t
  :config
  (define-key prog-mode-map "\C-x\C-n" #'forward-page)
  (define-key prog-mode-map "\C-x\C-p" #'backward-page))

(use-package ace-window
  :defer t
  :ensure t)

(use-package free-keys
  :ensure t
  :defer t
  :commands (free-keys))

(use-package helm
  :ensure t
  :diminish helm-mode
  :defer t
  :commands R
  :bind (("C-x C-u" . helm-M-x))
  :init (helm-mode 1)
  :config
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (progn
    (require 'helm-config)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-quick-update t
          helm-candidate-number-limit 100
          helm-buffers-favorite-modes '("js2-mode" "html-mode" "css-mode")
          helm-ff-auto-update-initial-value t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t))
  (use-package helm-ls-git
    :ensure t
    :chords ((";b". helm-browse-project)))
  (use-package helm-projectile
    :ensure t
    :chords (("PH" . helm-projectile-find-file-dwim))
    :commands R)
  
  :bind (;; ("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("M-I" . helm-imenu)
         ("C-x c o" . helm-occur)
         ("C-x c SPC" . helm-all-mark-rings)))


(use-package helpers
  :ensure nil
  :load-path "~/.emacs.d/lisp/"
  :pin manual
  :bind (([remap kill-region] . cut-line-or-region)
        ([remap kill-ring-save] . copy-line-or-region)
        ("C-S-y" . duplicate-current-line-or-region)
        ("C-)" . my-increment-number-decimal)
        ("C-)" . my-increment-number-decimal)
        ("M-n" . toggle-comment-on-line))
  :chords ((";d" . line-below)
           (";u" . line-above)
           ("JJ" . switch-to-previous-buffer)
           ("PP" . paste-from-clipboard)
           ("CC" . copy-to-clipboard)))
;; (org-babel-load-file (concat user-emacs-directory "config.org"))
