(add-to-list 'load-path "~/.emacs.d/lisp/")
;; (add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/git/org-mode/lisp")
(add-to-list 'load-path "~/git/org-mode/contrib/lisp/")
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(require 'org-drill)
(require 'use-package)
(require 'recentf)
(require 'dired-x)
(require 'dired)
(require 'saveplace)

(smex-initialize)
(key-chord-mode 1)
(nyan-mode t)
(pallet-mode t)
(global-aggressive-indent-mode 1)
(yas-global-mode t)
(recentf-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode 1)
(global-visual-line-mode t)             ;don't break lines in the middle of words
(ido-mode -1)
(ido-everywhere 1)
(flx-ido-mode 1)
(delete-selection-mode t)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode t)
(when (window-system)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

;; (edit-server-start 1)

;; I have N cards.
;; * Q1                                   :drill:EN:
;; * Q2                                   :drill:EN:
;; * Q3                                   :drill:JP:
;; Today I like study only EN cards. How I can do this? --> 

(defun org-drill-english-only (&optional scope)
  (interactive)
  (org-drill scope "+EN"))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; org stuff
(setq org-log-done t)
(setq org-agenda-files (list "~/orgmode/todo.org"))
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; (global-set-key (kbd "M-x") 'smex)
(global-set-key "\C-x\C-u" 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(setq user-full-name "Farhad Hassani"
      user-mail-address "farhad.hsni@gmail.com")

(defun jquery-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("jQuery") ) )

(add-hook 'js2-mode-hook 'jquery-doc)
(setq helm-dash-common-docsets '("JavaScript"))


(setq scroll-margin 1
      scroll-conservatively 9999
      scroll-step 1)                    ;smooth scrolling
(toggle-indicate-empty-lines t)

(fset 'node2
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 104 134217852 110 111 100 101 return 21 67108923 21 67108923] 0 "%d")) arg)))

(fset 'prevBuffer
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 98 return] 0 "%d")) arg)))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; (use-package dash)
(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ;; ("C-x C-b" . helm-buffers-list)
         ;; ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-to-list 'load-path "/usr/lib/node_modules/tern/emacs")
(add-to-list 'load-path "/home/farhad/node_modules/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(setq multi-term-program "/usr/bin/zsh")
(setq key-chord-two-keys-delay 0.2)
(setq key-chord-one-key-delay 0.3)
(key-seq-define-global ";f"  'iy-go-to-char)
(key-seq-define-global ";t"  'iy-go-up-to-char)
(key-seq-define-global ";a"  'mark-whole-buffer)
(key-seq-define-global ";d"  'line-below)
(key-seq-define-global ";u"  'line-above)
(key-seq-define-global ";c"  'copy-to-clipboard)
(key-seq-define-global ";p"  'paste-from-clipboard)
(key-seq-define-global ";s"  'helm-dash)
(key-seq-define-global ";i"  'er/mark-js-if)
(key-seq-define-global ";h"  'hs-toggle-hiding)
(key-seq-define-global ";g"  'magit-status)

(use-package multiple-cursors
  :bind (( "M-]" . mc/mark-next-like-this)
         ( "M-[" . mc/mark-previous-like-this)
         ( "M-A" . mc/mark-all-like-this)))
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(defun paste-from-clipboard ()
  (interactive)
  (setq x-select-enable-clipboard t)
  (yank)
  (setq x-select-enable-clipboard nil))
(defun copy-to-clipboard()
  (interactive)
  (setq x-select-enable-clipboard t)
  (kill-ring-save (region-beginning) (region-end))
  (setq x-select-enable-clipboard nil))

(setq recentf-max-menu-items 25)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(setq emmet-move-cursor-between-quotes t)
                                        ;for apropos to show results by relevancy
(setq apropos-sort-by-scores t)
;; disable eletric-indent for org-mode
(add-hook 'org-mode-hook (lambda()      
                           (set (make-local-variable 'electric-indent-functions)
                                (list (lambda(org) 'no-indent)))))
(setq org-src-fontify-natively t)
;; (set-frame-font "SourceCodePro-13")
;; (set-frame-font "Operator Mono XLight-13")
;; (set-frame-font "Operator Mono Book-12")
(setq default-frame-alist '((font . "Operator Mono Book-12")))

(setq ido-use-faces nil)
(setq ido-enable-flex-matching t)
(global-set-key [remap dabbrev-expand] 'hippie-expand) ;use hippie-expand instead of dabbrev-expand
(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))
                                        ;keyboard bindings

(fset 'commentCurrentLine
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 67108923 5 134217787] 0 "%d")) arg)))

(global-set-key (kbd "M-F") 'er/mark-js-function)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<f7>") 'prevBuffer)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "<f8>") 'node2) ;
(global-set-key (kbd "C-{") 'er/mark-inside-pairs) ;
(global-set-key (kbd "C-}") 'er/mark-inside-pairs) ;
(global-set-key (kbd "C-*") 'er/mark-inside-quotes) ;
(global-set-key (kbd "M-n") 'commentCurrentLine) ;macro for commenting current line
(global-set-key (kbd "RET") 'newline-and-indent) ;no need for emacs 24.4 and above
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-,") 'ace-jump-mode)
(global-set-key (kbd "C-.") 'create-snippet)
;; (global-set-key [(control return)] 'line-above)
(global-set-key (kbd "C-S-y") 'duplicate-current-line-or-region)
(global-set-key (kbd "<f5>") 'save-buffer)
(global-set-key (kbd "<f6>") 'recentf-open-files)
;; (global-set-key (kbd "<f7>") 'overwrite-mode)
(global-set-key (kbd "C-)") 'my-increment-number-decimal)
(global-set-key (kbd "C-(") 'my-decrement-number-decimal)
(global-set-key (kbd "M-p") 'other-window)
(global-set-key (kbd "M-L") 'flycheck-list-errors)
(global-set-key (kbd "C->") 'emmet-next-edit-point)
(global-set-key (kbd "C-<") 'emmet-prev-edit-point)
(global-set-key (kbd "C-;") 'set-mark-command)
(global-set-key (kbd "M-I") 'helm-imenu)
(global-set-key (kbd "C-z") 'zop-up-to-char)
(global-set-key (kbd "C-M-m") 'flycheck-mode)
;; (global-unset-key (kbd "C-z"))          ;c-z do nothing

(global-set-key [remap kill-region] 'cut-line-or-region) 
(global-set-key [remap kill-ring-save] 'copy-line-or-region)

(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f
                        (keyboard-translate ?\C-j ?\C-x)
                        (keyboard-translate ?\C-x ?\C-j))))

(keyboard-translate ?\C-j ?\C-x)
(keyboard-translate ?\C-x ?\C-j)

;; DIRED SETTINGS
(setq dired-recursive-deletes (quote top))
(define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^") (lambda ()
				       (interactive)
				       (find-alternate-file "..")))

(defun create-snippet (filename)
  "Creates snippet file in ~/.emacs.d/snippets/<mode-name> folder"
  (interactive "s")
  (let ((mode (symbol-name major-mode)))
    (find-file (format "~/.emacs.d/snippets/%s/%s" mode filename))
    (snippet-mode)))
(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))
(defun my-decrement-number-decimal (&optional arg)
  (interactive "p*")
  (my-increment-number-decimal (if arg (- arg) -1)))
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (beginning-of-visual-line)
        (insert region)
        (indent-according-to-mode)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun cut-line-or-region()
  "Kill current line if no region is active, otherwise kills region."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))
(defun copy-line-or-region()
  "Copy current line if no region is active, otherwise copies region."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))

(defun line-below()
  "make a new line below current line"
  (interactive)
  (end-of-visual-line nil)
  (newline))
                                        ;save position of cursor----------------------------
;; (setq save-place-file (concat user-emacs-directory "~/emacs/saveplace.el")) ;save it on emacs directory under the name of saveplace.el
(setq-default save-place t)
;; (require 'auto-complete-config)         
(ac-config-default)
;; (setq ac-auto-start 4)
(define-key ac-completing-map "\r" nil) ;don't use enter plz
(setq ac-auto-show-menu 0.3)
(defun select-current-line ()
  "Selects the current line"
  (interactive)
  (end-of-line)
  (push-mark (line-beginning-position) nil t))
(defun line-above ()
  "make a new line above current line"
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))
(setq backup-directory-alist `(("." . "~/emacs/backup_files"))) ;save backup files here
;; (setq make-backup-file nil)                         ;disable backup files
(setq auto-save-default nil)                        ;disable auto-save
(setq inhibit-startup-message t)                    ;disable startup message
(setq-default tab-width 2)                          ;two space per tab
(setq-default indent-tabs-mode nil)                 ;use spaces for tabs
(fset 'yes-or-no-p 'y-or-n-p)                       ;accept y and n for yes and no
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
