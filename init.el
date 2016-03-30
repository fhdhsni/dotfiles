(add-to-list 'load-path "~/.emacs.d/lisp/")
;; Using Cask for Package Management
(setq evil-want-C-u-scroll t)
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; (add-to-list 'load-path "~/git/org-mode/lisp")
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq multi-term-program "/usr/bin/zsh")

(setq exec-path (cons (expand-file-name "/usr/local/bin/sass") exec-path))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


(key-chord-mode 1)
(key-chord-define-global ";;"  'end-of-visual-line)

(setq ns-pop-up-frames nil)

(require 'multiple-cursors)
(global-set-key (kbd "M-]") 'mc/mark-next-like-this)
(global-set-key (kbd "M-[") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-A") 'mc/mark-all-like-this)

(add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)
(global-set-key (kbd "C-c t") 'iy-go-up-to-char)
(global-set-key (kbd "C-c T") 'iy-go-up-to-char-backward)


;; (eval-after-load 'js2-mode
;;   '(define-key js2-mode-map (kbd "RET") 'js2-line-break))

(global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

(add-hook 'after-init-hook 'global-color-identifiers-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(evil-disable-insert-state-bindings t)
 '(fci-rule-color "#515151")
 '(initial-frame-alist (quote ((fullscreen . maximized))))
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

                                        ;web-mode stuff
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; (relative-line-numbers-mode t)
;; (line-number-mode t)
;; (column-number-mode t)

                                        ;evil stuff

;; Keymaps are modified with the Emacs function 'define-key':

;; (define-key evil-normal-state-map "w" 'foo)

;; This binds the key 'w' to the command 'foo' in Normal state.  The file
;; 'evil-maps.el' contains all the key bindings.
(evil-mode 1)
(global-evil-surround-mode 1)
(global-evil-matchit-mode 1)
 (defun my-esc (prompt)
     "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
     (cond
      ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
      ;; Key Lookup will use it.
      ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
      ;; This is the best way I could infer for now to have C-c work during evil-read-key.
      ;; Note: As long as I return [escape] in normal-state, I don't need this.
      ;;((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
      (t (kbd "C-g"))))
 (define-key key-translation-map (kbd "C-g") 'my-esc)
   ;; Works around the fact that Evil uses read-event directly when in operator state, which
   ;; doesn't use the key-translation-map.
   (define-key evil-operator-state-map (kbd "C-g") 'keyboard-quit)
   ;; Not sure what behavior this changes, but might as well set it, seeing the Elisp manual's
   ;; documentation of it.
(set-quit-char "C-g")
(setq evil-move-cursor-back nil)
;end of evil stuff

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "f" 'find-file)
(evil-leader/set-key
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "s" 'save-buffer
  "h" 'mark-whole-buffer
  "w" 'copy-to-clipboard
  "y" 'paste-from-clipboard;
  "c" 'save-buffers-kill-terminal)

;; (Setq httpd-root "/home/farhad/bin/exercise2/")
;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; (add-hook 'html-mode-hook 'skewer-html-mode)
;; (require 'dot-mode)
;; (add-hook 'find-file-hooks 'dot-mode-on)
(yas-global-mode t)
;keep a list of recently opened files
(require 'recentf)
(recentf-mode 1)
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

;(set-frame-font "Fira Mono for Powerline-11")
(set-frame-font "Droid Sans Mono Dotted for Powerline-12")

(when (window-system)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1))
(menu-bar-mode -1)
(electric-pair-mode 1)       ;insert right brackets when left one is typed
;(global-hl-line-mode -1) ; turn on highlighting current line
(global-visual-line-mode t)             ;don't break lines in the middle of words
(ido-mode 1)                            ;kind of fuzzy search for buffer and find files
(ido-everywhere 1)                      ;kind of fuzzy search for buffer and find files
(flx-ido-mode 1)                        ;kind of fuzzy search for buffer and find files
(setq ido-use-faces nil)

                                        ;keyboard bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-'") 'er/expand-region)
(global-set-key (kbd "M-n") 'ace-jump-mode)
(global-set-key (kbd "M-i") 'select-current-line)
(global-set-key (kbd "C-.") 'create-snippet)
;; (global-set-key (kbd "C-;") 'emmet-expand-line) 
(global-set-key (kbd "M-RET") 'line-below)
(global-set-key (kbd "M-O") 'line-above)
;; (global-set-key [(control return)] 'line-above)
(global-set-key (kbd "C-S-y") 'duplicate-current-line-or-region)
(global-set-key (kbd "<f5>") 'save-buffer)
(global-set-key (kbd "<f6>") 'recentf-open-files)
(global-set-key (kbd "<f7>") 'overwrite-mode)
(global-set-key (kbd "C-)") 'my-increment-number-decimal)
(global-set-key (kbd "C-(") 'my-decrement-number-decimal)
(global-set-key (kbd "M-p") 'other-window)
(global-set-key (kbd "M-P") 'flycheck-previous-error)
(global-set-key (kbd "M-N") 'flycheck-next-error)
(global-set-key (kbd "M-L") 'flycheck-list-errors)
(global-set-key (kbd "C->") 'emmet-next-edit-point)
(global-set-key (kbd "C-<") 'emmet-prev-edit-point)
(global-set-key (kbd "M-?") 'undo)


;; CUSTOM FUNCTIONS                        
(global-set-key [remap kill-region] 'cut-line-or-region) 
(global-set-key [remap kill-ring-save] 'copy-line-or-region)

(keyboard-translate ?\C-j ?\C-x)
(keyboard-translate ?\C-x ?\C-j)
                                        ;(keyboard-translate ?\C-l ?\C-c)
                                        ;(keyboard-translate ?\C-c ?\C-l)



;; DIRED SETTINGS
(require 'dired)
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
  (newline-and-indent))

;save position of cursor----------------------------
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace.el")) ;save it on emacs directory under the name of saveplace.el
(setq-default save-place t)
;end of save position--------------------------------

;autoindent------------------------------------
(electric-indent-mode t)
;end of autoindent---------------------------

(require 'powerline)
(powerline-default-theme)
(setq powerline-color1 "gray30")
(setq powerline-color2 "gray45")
(set-face-attribute 'mode-line nil
                    :background "gray22"
                    :foreground "F0DFAF"
                    :box nil)
(powerline-center-evil-theme)

                                        ;autocomplete
(require 'auto-complete-config)
(ac-config-default)
                                        ;end
                                        ;select current line function
(defun select-current-line ()
  "Selects the current line"
  (interactive)
  (end-of-line)
  (push-mark (line-beginning-position) nil t))
                                        ;end

                                        ;make a new line above current line
(defun line-above ()
  "make a new line above current line"
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))
                                        ;end


(delete-selection-mode t)               ;delete selection region when start typing
(blink-cursor-mode -1)                  ;don't blink
(show-paren-mode t)                     ;highlight maching brackets 

(setq backup-directory-alist `(("." . "~/.saves"))) ;save backup files here
(setq make-backup-file nil)                         ;disable backup files
(setq auto-save-default nil)                        ;disable auto-save
(setq inhibit-startup-message t)                    ;disable startup message
(setq-default tab-width 2)                          ;two space per tab
(setq-default indent-tabs-mode nil)                 ;use spaces for tabs
(fset 'yes-or-no-p 'y-or-n-p)                       ;accept y and n for yes and no

;; PROJECTILE
;; (projectile-global-mode)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
