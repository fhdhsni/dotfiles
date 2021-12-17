(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(setq user-full-name "Farhad Hasani"
      user-mail-address "farhad.hsni@gmail.com")

(defun fhd/load-user-file (file)
  "Load a FILE in current user's configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))

(defun fhd/dq ()
  "Insert double quotes in an org buffer."
  (interactive)
  (insert "“”")
  (backward-char))

(defun fhd/line-above ()
  "Make a new line above current line."
  (interactive)
  (move-beginning-of-line nil)
  ;; (newline-and-indent)
  (newline)
  (forward-line -1))


(defun xah-insert-random-hex (NUM)
  "Insert NUM random hexadecimal digits.
NUM default to 6.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2017-08-03"
  (interactive "P")
  (let (($n (if (numberp NUM) (abs NUM) 6 )))
    (format  (concat "%0" (number-to-string $n) "x" ) (random (1- (expt 16 $n))))))

(defun fhd/random-bg-css ()
  "Insert background color with a random color"
  (interactive)
  (insert (concat "#" (xah-insert-random-hex 6) ";")))

(defun fhd/line-below()
  "make a new line below current line"
  (interactive)
  (end-of-visual-line nil)
  (newline))

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

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun fhd/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1))
  (minibuffer-line--update))

(defun fhd/toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun fhd/delete-up-to-word (word)
  "Delete up to a given WORD."
  (interactive "s")
  (let ((beg (point)))
    (word-search-forward word)
    (delete-region beg (- (point) (length word))))
  (backward-char (length word)))

(defun fhd/toggle-window-split ()
  "Switch between vertical and horizontal window."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun fhd/open-in-gui-filemanager ()
  "Open current directry in filermanager."
  (interactive)
  (let ((process-connection-type nil)
        (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                              "/usr/bin/gvfs-open"
                            "/usr/bin/xdg-open")))
     (start-process "" nil "konsole" ".")))

(defun fhd/open-in-external-app ()
  "Pass the file under point to xdg-open."
  (interactive)
  (let* ((-file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (-do-it-p (if (<= (length -file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when -do-it-p
      (mapc
       (lambda (-fpath) (let ((process-connection-type nil))
                           (start-process "" nil "open" -fpath))) -file-list))))

(defun fhd/switch-to-last-window ()
  "Switch to previous window."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

(defun fhd/find-file-recursivly (str)
  "Recursively find files containing STR with `directory-files-recursively'."
  (interactive "sGimme a Pattern: ")
  (ivy-read "File: " (directory-files-recursively "." str t)))

(defun fhd/open-init ()
  "Open init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun fhd/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
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
      (goto-char (+ origin (* (length region) arg) arg))))
  (let ((ending-word (save-excursion (end-of-visible-line) (word-at-point))))
    (if (and (string-equal "do" ending-word) (string-equal (symbol-name major-mode) "elixir-mode"))
        (progn
          (forward-line -1)
          (end-of-visual-line)
          (kill-word -1)
          (whitespace-cleanup)))))

(defun fhd/region-to-bottom ()
  "Set mark from point to bottom of the page"
  (interactive)
  (cua-set-mark)
  (end-of-buffer)
  (exchange-point-and-mark))

(defun fhd/region-to-top ()
  "Set mark from point to top of the page"
  (interactive)
  (cua-set-mark)
  (beginning-of-buffer)
  (exchange-point-and-mark))

(defun fhd/change-quote (start end)
  "Change ' to \" or vise-versa"
  (interactive "r")
  (save-excursion
    (let ((new-quote
           (if (= (char-after start) ?\")
               ?\'
             ?\")))
      (goto-char end)
      (delete-char -1)
      (insert new-quote)
      (goto-char start)
      (delete-char 1)
      (insert new-quote))))

(defun fhd/surround-with-equal (start end)
  "Surround the region under the point with equal sign."
  (interactive "r")
  (save-excursion
    (goto-char end) (insert "~")
    (goto-char start) (insert "~")))

(defun fhd/surround-with-bracket (start end)
  "Surround the region under the point with bracket."
  (interactive "r")
  (save-excursion
    (goto-char end) (insert "<")
    (goto-char start) (insert ">")))


(defun fhd/surround-the-word-under-point-with-equal (start end)
  (interactive "r")
  (cond ((and (use-region-p) (boundp 'start))
         (fhd/surround-with-equal start end))
        (t (let (bounds pos1 pos2 mything)
             (setq bounds (bounds-of-thing-at-point 'symbol))
             (setq pos1 (car bounds))
             (setq pos2 (cdr bounds))
             ;; (setq mything (buffer-substring-no-properties pos1 pos2))
             (fhd/surround-with-equal pos1 pos2)))))


(defun fhd/create-tag ()
  (interactive)
  (let (bounds pos1 pos2 mything)
    (setq bounds (bounds-of-thing-at-point 'symbol))
    (setq pos1 (car bounds))
    (setq pos2 (cdr bounds))
    (setq mything (buffer-substring-no-properties pos1 pos2))
    (goto-char pos1)
    (kill-word 1)
    (insert (concat "<" ) mything "></" mything ">")))

;; https://github.com/coldnew/coldnew-emacs/blob/master/init.org
(defun fhd/swap-window-positions ()
  "*Swap the positions of this window and the next one."
  (interactive)
  (let ((other-window (next-window (selected-window) 'no-minibuf)))
    (let ((other-window-buffer (window-buffer other-window))
          (other-window-hscroll (window-hscroll other-window))
          (other-window-point (window-point other-window))
          (other-window-start (window-start other-window)))
      (set-window-buffer other-window (current-buffer))
      (set-window-hscroll other-window (window-hscroll (selected-window)))
      (set-window-point other-window (point))
      (set-window-start other-window (window-start (selected-window)))
      (set-window-buffer (selected-window) other-window-buffer)
      (set-window-hscroll (selected-window) other-window-hscroll)
      (set-window-point (selected-window) other-window-point)
      (set-window-start (selected-window) other-window-start))
    (select-window other-window)))

(defun fhd/enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

(defun fhd/jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun fhd/find-file-and-close-other-window (file)
  (find-file file)
  (delete-other-windows))

(defun fhd/shortened-path (path max-len)
  "Return a modified version of `PATH', replacing some components
      with single characters starting from the left to try and get
      the path down to `max-len'"
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(defun fhd/capitalize-previous-word ()
  (interactive)
  (capitalize-word -1))

(defun fhd/copy-word-under-point ()
  (interactive)
  (let (bounds pos1 pos2 mything)
    (setq bounds (bounds-of-thing-at-point 'symbol))
    (setq pos1 (car bounds))
    (setq pos2 (cdr bounds))
    (setq mything (buffer-substring-no-properties pos1 pos2))
    (kill-new mything)
    (message "copied: %s" mything)))

(defun fhd/bjm-comment-box (b e)
  "Draw a box comment around the region but arrange for the region to extend to at least the fill column. Place the point after the comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))


(straight-use-package 'use-package)

(use-package straight
             :custom (straight-use-package-by-default t))

(use-package auto-compile
  :straight t
  :demand t
  :config (auto-compile-on-load-mode))

(use-package bind-key
  :straight t)

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
 :demand t
 :straight nil
 :init
 (setq custom-file "~/.emacs.d/custom.el"))

(use-package ibuffer
  :defer t
  :straight t
  :bind* (("C-x l" . ibuffer)))

(use-package hideshow
  :bind ("C-c h" . hs-toggle-hiding)
  :straight t
  :commands hs-toggle-hiding
  :defer t)

(use-package ispell
  :straight t
  :bind (("C-c s b" . ispell-buffer)
         ("C-c s w" . ispell-word)))

(use-package delight
  :straight t)

(use-package eldoc
  :straight t
  :delight
  :defer t
  :diminish eldoc-mode
  :init (add-hook 'prog-mode-hook 'eldoc-mode)
  :commands eldoc-mode)

(use-package autorevert
  :straight t
  :delight)

(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

(use-package auto-compile
  :straight t
  :config (auto-compile-on-load-mode))

(fset 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)


(setq
 mac-option-modifier 'super
 mac-command-modifier 'meta
 ring-bell-function 'ignore
 default-directory "~/"
 command-line-default-directory "~/"
 initial-scratch-message ""
 ;; frame-title-format '("" "%b ")
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
 ;; default-frame-alcist '((font . "Fira Code Medium-17") (cursor-type . bar))
 ;; default-frame-alist '((font . "Operator Mono Book-18") (cursor-type . box))
 default-frame-alist '((font . "JetBrains Mono-15") (cursor-type . box))
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
;; (fringe-mode '(1 . 0))
;; (fringe-mode '(0 . 0))
(fringe-mode '(4 . 4))
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
(global-hl-line-mode -1)
(display-time-mode 1)
(global-auto-revert-mode 1)
(cua-mode)
(global-subword-mode)
(setq-default
 tab-width 4
 indent-tabs-mode nil)

(setq-default cursor-type 'box)

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
(global-set-key (kbd "M-*") 'quick-calc)
(global-set-key (kbd "M-n") 'mark-word)
(global-set-key (kbd "<f12>") #'toggle-frame-fullscreen)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'fhd/show-buffer-info)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-c o") 'ace-window)
(global-set-key (kbd "s-t") 'fhd/nothing)
(global-set-key (kbd "M-p") 'goto-line)
(global-set-key (kbd "C-+") 'fill-paragraph)
(global-set-key (kbd "C-c C-n")  'fhd/toggle-comment-on-line)
(global-set-key (kbd "C-c i")  'fhd/open-init)
(global-set-key (kbd "C-c C-j")  'fhd/jump-to-mark)
(global-set-key (kbd "C-)")  'fhd/region-to-bottom)
(global-set-key (kbd "C-c k")  'fhd/random-bg-css)
(global-set-key (kbd "C-(")  'fhd/region-to-top)
(global-set-key (kbd "C-c C-=")  'fhd/surround-the-word-under-point-with-equal)
(global-set-key (kbd "C-c C--")  'fhd/surround-the-word-under-point-with-equal)
(global-set-key (kbd "M-s y")  'fhd/copy-word-under-point)
(global-set-key (kbd "C-c t")  'fhd/create-tag)
(global-set-key (kbd "C-c C-d")  'fhd/duplicate-current-line-or-region)
(global-set-key (kbd "C-x '")  'fhd/switch-to-last-window)

(defun fhd/nothing ()
  (interactive)
  (message "nothing"))

(use-package marginalia
  :straight t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode)
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit 'keep-selected)))))

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

;; (global-set-key [remap dabbrev-expand] 'hippie-expand) ;use hippie-expand instead of dabbrev-expand

(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f
                        (scroll-bar-mode -1)
                        (toggle-frame-maximized)
                        (keyboard-translate ?\C-j ?\C-x)
                        (keyboard-translate ?\C-x ?\C-j))))

(keyboard-translate ?\C-j ?\C-x)
(keyboard-translate ?\C-x ?\C-j)

(use-package async
  :straight t
  :defer t
  :config
  (setq async-bytecomp-package-mode t))

(use-package define-word :defer t
  :straight t)

(use-package smartparens
  :straight t
  :demand t
  :init
  (require 'smartparens-config)
  :bind* (("C-, C-a" . sp-beginning-of-sexp)
          ("M-a" . sp-beginning-of-sexp)
          ("C-, C-e" . sp-end-of-sexp)
          ("M-e" . sp-end-of-sexp)
          ("C-, C-d" . sp-down-sexp)
          ("C-, C-u" . sp-up-sexp))
  :bind (("M-?" . sp-unwrap-sexp)
         ("C-}" . sp-beginning-of-next-sexp)
         ("C-{" . sp-beginning-of-previous-sexp)
         ("C-|" . sp-forward-slurp-sexp)
         ("C-c C-\"" . sp-up-sexp)
         ("C-:" . sp-forward-barf-sexp)))

(use-package multiple-cursors
  :straight t
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
  :straight t
  :defer t
  :bind (("C-c C-z" . zop-up-to-char)))

(use-package guide-key
  :straight t
  :delight
  :defer t
  :diminish guide-key-mode
  :init
  (progn
    (setq guide-key/guide-key-sequence
          '("C-c" "C-c t" "C-c s" "C-c h" "C-c p" "C-h" "C-," "C-, C-c" "C-x" "C-x a" "C-x 4" "C-x r" "C-x v" "M-s" "M-o" "M-g"))
    (guide-key-mode 1)))

(use-package guide-key-tip
  :straight t
  :after guide-key
  :init
  (setq guide-key-tip/enabled t))

(use-package emmet-mode
  :straight t
  :delight
  :defer t
  :bind (("C->" . emmet-next-edit-point)
         ("C-<" . emmet-prev-edit-point)
         ("M-E" . emmet-expand-yas))
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  :config
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (setq emmet-move-cursor-between-quotes t))

(use-package expand-region
  :straight t
  :demand t
  :bind* (("C-c C-'" . er/mark-inside-quotes)
          ("C-c C-9" . er/mark-inside-pairs)
          ("C-c f" . er/mark-defun)
          ("C-c C-0" . er/mark-outside-pairs)
          ;; ("C-i" . er/expand-region)
          ("C-c j" . er/expand-region)))

(use-package diff-hl
  :straight t
  :config
  (diff-hl-amend-mode)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package flycheck
  :straight t
  :bind (("C-M-m" . flycheck-mode)
         ("M-L" . flycheck-buffer)
         ("C-c u" . flycheck-list-errors))
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

(use-package yasnippet
  :straight t
  :defer 15
  :delight
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
  :straight t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package haskell-mode
  :straight t
  :defer t)


(use-package magit
  :straight t
  :defer nil
  :bind* (("C-c C-m" . magit-status))
  :bind (:map magit-diff-mode-map
              ("," . magit-diff-visit-worktree-file-other-window)
              ("." . magit-diff-visit-file-other-window))
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

(use-package recentf
  :straight t
  :commands (recentf-open-files)
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 50))

(use-package scss-mode
  :straight t
  :defer t
  ;; :hook
  ;; (sass-mode . prettier-js-mode)
  ;; (css-mode . prettier-js-mode)
  :mode "\\.scss\\'"
  :config (setq indent-tabs-mode nil tab-width 2))


(use-package json-mode
  :straight t
  :defer t
  :mode "\\.json\\'"
  :config
  (flycheck-mode)
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

(use-package avy
  :straight t
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
  :straight t
  :after avy
  :bind (("M-z" . zap-up-to-char)))

(use-package ace-window
  :straight t
  :bind* (("C-x o" . ace-window))
  :custom
  (aw-keys '(?a ?n ?d ?f ?g ?h ?j ?k ?l)))

(use-package free-keys :commands (free-keys)
  :straight t)

(use-package bm
  :straight t)

(use-package web-mode
  :straight t
  :delight
  :defer t
  :hook
  (web-mode . tide-mode)
  (web-mode . flycheck-mode)
  (web-mode . emmet-mode)
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (flymake-mode 0)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 0)
  (setq web-mode-css-padding 0)
  (setq web-mode-block-padding 0)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-style-padding 0)
  (setq web-mode-markup-indent-offset 2))

(use-package shell-pop
  :straight t
  :defer t
  :bind* (("C-c C-s" . shell-pop))
  :init
  (setq
   shell-pop-term-shell "eshell"
   shell-pop-shell-type '("eshell" "eshell" (lambda () (eshell)))
   shell-pop-universal-key "C-c C-s"
   shell-pop-window-size 50
   shell-pop-window-position "right"))

(use-package quickrun
  :straight t
  :bind ("C-c x" . quickrun))

(use-package yaml-mode :defer t
  :straight t)

(use-package move-text
  :straight t
  :defer t
  :init
  (move-text-default-bindings))

(use-package find-file-in-project
  :straight t
  :defer t
  :init
  (setq ffip-use-rust-fd t))

(use-package ruby-end
  :straight t
  :defer t)

(use-package exunit
  :straight t)

(use-package elixir-mode
  :straight t
  :diminish elixir-mode
  :delight
  :hook
  (elixir-mode . lsp)
  (elixir-mode . exunit-mode)
  (elixir-mode . smartparens-mode)
  (elixir-mode . show-smartparens-mode)
  (elixir-mode . ruby-end-mode)
  :init
  (add-to-list 'exec-path "~/repos/elixir-ls/release")
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


(use-package erlang
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.config\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\.config.script\\'" . erlang-mode))
  (add-to-list 'exec-path "~/erlang_ls/_build/default/bin")
  ;; Enable LSP automatically for Erlang files
  (add-hook 'erlang-mode-hook #'lsp)
  :defer t
  :bind (:map erlang-mode-map
              ("C-`" . ivy-erlang-complete)
              ("C-C a" . fhd/capitalize-previous-word)
              ("C-c C-k" . fhd/compile-and-display-erlang))
  :config

  ;; (setq load-path (cons  "/usr/lib/erlang/lib/tools-3.0.1/emacs" load-path))
  ;; (setq erlang-root-dir "/usr/lib/erlang")
  ;; (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
  ;; (setq erlang-man-root-dir "/usr/lib/erlang/man")
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

(use-package writeroom-mode
  :straight t
  :defer t)

(use-package mmm-mode
  :straight t)

(use-package dumb-jump
  :straight t
  :defer t
  :bind (("C-c g" . dumb-jump-go))
  :config
  (setq dumb-jump-selector 'ivy))


(use-package helpful
  :straight t
  :config
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

(use-package fold-this
  :straight t
  :bind (:map fold-this-keymap
              ("c-c f" . fold-this))
  :defer t)

(use-package easy-kill
  :straight t
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill))


(use-package ag
  :straight t
  :defer t)

(use-package treemacs
  :straight t
  :defer t
  :bind (("<f8>" . treemacs)
         ("<C-tab>" . treemacs-select-window))
  :init
  ;; (add-hook 'treemacs-mode-hook (lambda () (text-scale-decrease 1)))
  (with-eval-after-load "treemacs"
    (setq
     treemacs-position 'left
     treemacs-icon-fallback-text (propertize "> " 'face 'font-lock-keyword-face)
     treemacs-icon-tag-node-open-png   (propertize "− " 'face 'font-lock-keyword-face)
     treemacs-icon-tag-node-closed-png (propertize "+ " 'face 'font-lock-keyword-face)
     treemacs-icon-tag-leaf-png        (propertize "> " 'face 'font-lock-keyword-face))
    (setq treemacs-icon-tag-leaf-text (propertize "> " 'face 'font-lock-constant-face)))
  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-file-event-delay           10000
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            nil
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

(use-package indent-guide
  :straight t
  :delight
  :init
  (indent-guide-global-mode)
  (setq indent-guide-char "|")
  (setq indent-guide-delay 0.3))

(use-package highlight-symbol
  :straight t
  :init
  (global-set-key (kbd "C-!") #'highlight-symbol-at-point)
  (highlight-symbol-mode t)
  (highlight-symbol-nav-mode t))

(use-package unicode-fonts
   :straight t
   :config
    (unicode-fonts-setup))

(use-package autoinsert
  :straight t
  :init
  (add-hook 'find-file-hooks 'auto-insert)
  (setq auto-insert-alist
        '(("\\.scm" .
           (insert "#!/bin/sh\n#| -*- scheme -*-\nexec csi -s $0 \"$@\"\n|#\n")))))

(use-package lispy
  :hook ((scheme-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode))
  :init
  (with-eval-after-load "lispy"
    ;; (setq prettify-symbols-alist '(("lambda" . ?λ)))
    ;; (prettify-symbols-mode)
    (setq lispy-no-permanent-semantic t)))

(use-package volatile-highlights
  :straight t
  :delight
  :init (volatile-highlights-mode t))

(use-package toml-mode
  :straight t
  :defer t)

(use-package smart-mode-line
  :straight t
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (setq sml/theme 'dark))

(use-package phi-search
  :straight t)

(use-package prettier-js
  :straight t
  :delight
  :bind (("C-c C-p" . prettier-js))
  :init
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (add-hook 'web-mode-hook #'prettier-js-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . prettier-js-mode))
  ;; (setq prettier-js-args '(
  ;;                          "--trailing-comma" "all"
  ;;                          "--jsx-bracket-same-line" "true"
  ;;                          "--print-width" "100"
  ;;                          "--tab-width" "2"
  ;;                          "--single-quote" "true"
  ;;                          ))
  ;; (add-hook 'typescript-mode-hook 'prettier-js-mode)
  )
;; (setq prettier-js-args '("--parser" "typescript" "--trailing-comma" "all" "--print-width" "80" "--bracket-spacing" "true"))

(use-package add-node-modules-path
  :straight t
  :config
  '(progn
     (add-hook 'js-mode-hook #'add-node-modules-path)
     (add-hook 'typescript-mode-hook #'add-node-modules-path)))





(use-package deadgrep
  :straight t
  :config
  (global-set-key (kbd "<f5>") #'deadgrep)
  (global-set-key (kbd "C-#") #'deadgrep)
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
(defalias 'wm 'web-mode)
(defalias 'tr 'tide-restart-server)
(defalias 'el 'emacs-lisp-mode)

(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" nil))
    ad-do-it
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" agent))))

(use-package subword
  :straight t
  :delight)

(use-package selectrum
  :straight t
  :bind* (
          ("C-x C-a" . execute-extended-command)
          ("C-, C-r" . selectrum-repeat)
          ;; ("\C-x\C-n" . counsel-grep)
          ;; ("\C-x\C-n" . swiper) ;
          ;; ("C-." . swiper);
          ;; ("C-'" . ivy-switch-buffer)
          ;; ("C-, r" . eccounsel-recentf)
          ;; ("C-, l" . counsel-linux-app)
          ;; ("C-, m" . counsel-mark-ring)
          ;; ("C-, t" . fhd/counsel-tldr)
          ;; ("C-, c" . counsel-colors-web)
          ;; ("C-, e" . counsel-esh-history)
          ;; ("C-, C-f" . find-file-in-project)
          ;; ("M-I" . counsel-imenu)
          ;; ;; ("C-x C-u" . counsel-M-x)
          ;; ("C-x C-f" . counsel-explorer)
          ;; ("C-h b" . counsel-descbinds)
          ;; ("<f1> l" . counsel-find-library)
          ;; ("<f2> i" . counsel-info-lookup-symbol)
          ;; ("<f2> u" . counsel-unicode-char)
          ;; ("C-, g" . counsel-git)
          ;; ("C-, j" . counsel-git-grep)
          ;; ("C-, a" . counsel-rg)
          ;; ("C-, h" . counsel-apropos)
          ;; ("C-, C-l" . counsel-locate)
          ;; ("M-y" . counsel-yank-pop)
          )
  :init
  (selectrum-mode +1))
(use-package prescient
  :straight t)

(use-package selectrum-prescient
	     :straight t)

(use-package consult
  :straight t
  :bind* (
          ;; ("C-." . consult-line)
          ;; ("C-'" . consult-buffer)
          ;; ("M-y" . consult-yank-pop)
          ("C-, C-f" . find-file-in-project)
          ))

;; ;; to make sorting and filtering more intelligent
(selectrum-prescient-mode +1)

;; ;; to save your command history on disk, so the sorting gets more
;; ;; intelligent over time
(prescient-persist-mode +1)


(use-package lsp-mode
  :commands lsp
  :bind* (("C-, d" . lsp-describe-thing-at-point))
  :hook (go-mode . lsp-deferred)
  :config
  (flymake-mode 0)
  :init
  ;; (require 'lsp-clients)
  (defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'lsp-ui-mode-hook
	        (lambda ()
              (flymake-mode 0)
              (setq lsp-ui-sideline-enable nil)
              (setq lsp-ui-doc-enable nil)))
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-eldoc-render-all nil
        lsp-highlight-symbol-at-point nil)
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "/Users/farhad/rls-macos/reason-language-server")
  ;;                   :major-modes '(reason-mode)
  ;;                   :server-id 'ocaml-ls))
  )

(use-package lsp-ui
  :defer t
  :ensure t
  :config
  (setq lsp-ui-sideline-update-mode 'point))

(require 'lsp-ui-flycheck)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-after-open-hook (lambda ()
                                   (flymake-mode 0)
                                   (lsp-ui-flycheck-enable 1))))

(use-package org
  :mode ("\\.org" . org-mode)
  :defer 8
  :init
  (require 'org-tempo)
  :bind (("C-c l" . org-store-link)
         ("C-c C-," . counsel-org-goto)
         ("C-c C-x" . nil)
         ("C-c I" . org-insert-link))
  :config
  ;; (define-key org-mode-map (kbd "C-c C-x") nil)
  ;; (define-key org-mode-map (kbd "C-c C-,") 'counsel-org-goto)
  ;; (define-key org-mode-map (kbd "C-c I") 'org-insert-link)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  ;; (setq org-goto-interface 'outline)
  ;; (require 'ob-js)
  (setq org-modules
        '(org-bbdb
          org-bibtex
          org-docview
          org-gnus
          org-info
          org-irc
          org-mhe
          org-rmail
          org-w3m))
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  ;; (setq org-agenda-files (list "~/Dropbox/org/agenda.org"))
  (setq org-todo-keywords '((type
                             "TODO(t)"
                             "WAITING(w)"
                             "NEXT(n)"
                             "IN-PROGRESS(i)"
                             "SOMEDAY(s)"
                             "|"
                             "CANCELED(c)"
                             "DONE(d)")))

  (setq org-tag-alist '(("@WATCH" . ?w)
                        ("@READ" . ?r)
                        ("@PERSONAL" . ?p)
                        ("@MERGE" . ?m)
                        ("@CODE" . ?c)
                        ("@DRUPZ" . ?d)
                        ("@DRYE" . ?r)
                        ("@PHONE" . ?h)
                        ("@BUG" . ?g)
                        ("@URGENT" . ?e)))

  (add-hook 'org-mode-hook (lambda()
                             (company-mode)
                             (hl-line-mode -1)
                             (setq org-agenda-custom-commands
                                   (quote
                                    (("n" "Agenda and all TODOs"
                                      ((agenda "" nil)
                                       (alltodo "" nil))
                                      nil)
                                     ("i" "Important: Urgent things and phone calls"
                                      ((tags "@URGENT"
                                             ((org-agenda-overriding-header "Urgents things to do")))
                                       (tags "@PHONE"
                                             ((org-agenda-overriding-header "Phone calls to make"))))
                                      nil nil))))

                             (set (make-local-variable 'electric-indent-functions)
                                  (list (lambda(org) 'no-indent)))))
  (setq org-src-fontify-natively t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (java . nil)
     (shell . t)
     (js . t))))

(use-package js2-mode
  :defer t
  :diminish Javascript-IDE
  :mode "\\.js\\'"
  :load-path "~/.nvm/versions/node/v10.12.0/lib/node_modules/tern/emacs"
  :hook ((js2-mode . lsp)
         (js2-mode . hs-minor-mode))
  :init
  ;; (autoload 'tern-mode "tern.el" nil t)
  (add-hook 'js2-mode-hook #'(lambda ()
                               (prettier-js-mode t)
                               ;; (tern-mode t)
                               (flycheck-mode t)
                               ;; (setq prettify-symbols-alist '(("function" . ?ƒ)))
                               ;; (prettify-symbols-mode t)
                               (setq js2-mode-show-parse-errors nil)
                               (setq js2-mode-show-strict-warnings nil)
                               (setq js2-basic-offset 2)
                               (setq js2-strict-trailing-comma-warning nil))))

(use-package rjsx-mode
  :straight t
  :after js2-mode
  :hook
  ;; (rjsx-mode . prettier-js-mode)
  (rjsx-mode . tide-mode)
  (rjsx-mode . flycheck-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . rjsx-mode)))

(use-package eslint-fix :after js2-mode)

(use-package counsel
  :diminish ivy-mode
  :delight
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
          ;; ("C-x C-u" . counsel-M-x)
          ("C-x C-a" . counsel-M-x)
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

(use-package typescript-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.mjs\\'" . typescript-mode))
  :hook
  ;; (typescript-mode . lsp)
  (typescript-mode . flycheck-mode)
  (typescript-mode . tide-mode)
  :config
  ;; (prettier-js-mode)
  )

(use-package tide
  :defer t
  :bind (:map tide-mode-map
              ("C-c r" . tide-rename-symbol)
              ("C-c d" . tide-documentation-at-point)
              ("C-c C-t" . tide-fix))
  :config
  ;; (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
  (setq typescript-auto-indent-flag nil)
  (setq typescript-indent-level 2)
  (setq tide-always-show-documentation t)
  (tide-hl-identifier-mode)
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    ;; (prettier-js-mode)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)
    (hs-minor-mode)
    (add-to-list 'company-dabbrev-code-modes 'tide-mode))

  ;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  ;; (add-hook 'web-mode-hook
  ;;           (lambda ()
  ;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
  ;;               (setup-tide-mode))))
  ;; enable typescript-tslint checker
  ;; (flycheck-add-mode 'typescript-tslint 'web-mode)

  (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
  ;; (with-eval-after-load 'flycheck
  ;;   (flycheck-add-mode 'typescript-tslint 'tide-mode))

  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode))
(setq tide-format-options
      '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

(use-package dired
  :straight nil
  :bind* ("C-x C-j" . dired-jump)
  :config
  ;; (setq insert-directory-program "/usr/local/bin/gls")
  ;; (add-hook 'dired-mode-hook 'dired-filter-group-mode)
  ;; (add-hook 'dired-mode-hook 'dired-filter-by-dot-files)
  (define-key dired-mode-map (kbd "r") 'fhd/open-in-external-app)
  (define-key dired-mode-map (kbd "RET") 'fhd/open-in-external-app)
  (setq dired-listing-switches "-Ahl --time-style long-iso")
  ;; (setq dired-listing-switches "-Ahl")
  ;; (setq dired-listing-switches "-laGh1v --group-directories-first")
  (setq dired-listing-switches "-aBhl")

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
  (define-key dired-mode-map (kbd ";") 'dired-up-directory)
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

(use-package company
  :diminish company-mode
  :delight
  :demand t
  :bind (("C-c C-y" . company-complete)
         ("C-c y" . company-complete)
         ("C-`" . company-lsp))
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'elixir-mode-hook 'company-mode)
  (add-hook 'eshell-mode-hook 'company-mode)

  (require 'company-dabbrev)
  (use-package company-lsp
    :init
    (push 'company-lsp company-backends)
    (setq company-lsp-cache-candidates t
          company-lsp-async t))
  (bind-keys
   :map company-active-map
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
   company-minimum-prefix-length 2
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

(use-package ivy-hydra
  :defer t)

(custom-set-faces
 '(selectrum-prescient-primary-highlight ((t (:foreground "light green" :weight bold))))
 '(bm-face ((t (:background "turquoise4" :foreground "Black"))))
 '(cursor ((t (:background "orange"))))
 '(visible-mark-active ((t nil)))
 '(sml/vc-edited ((t (:slant oblique))))
 '(font-lock-comment-face ((t (:foreground "gray65" :slant italic :height 1.0))))
 ;; '(mode-line ((t (:box nil :background "grey80" :height 0.1))))
 ;; '(mode-line-inactive ((t (:box nil :background "grey30" :height 0.1))))
 '(mmm-default-submode-face ((t nil)))
 '(treemacs-root-face ((t (:inherit font-lock-constant-face :height 1.0))))
 ;; '(mode-line-buffer-id ((t (:foreground "darkmagenta"))))
 )

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


(set-face-attribute 'default (selected-frame) :height 130)

(use-package powerline
  :straight t
  :init
  (powerline-default-theme))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package fold-this
  :straight t)

(mmm-add-group
    'lit
    `((lit-variable
      :submode html-mode
      :face mmm-code-submode-face
      :front "html`"
      :back "`;")))

;; Main use is to have my key bindings have the highest priority
;; https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el
(defvar fhd-mode-map (make-sparse-keymap)
  "Keymap for `fhd-mode'.")

;;;###autoload
(define-minor-mode fhd-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-fhd-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " fhd-mode"
  :keymap fhd-mode-map)

;;;###autoload
(define-globalized-minor-mode global-fhd-mode fhd-mode fhd-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((fhd-mode . ,fhd-mode-map)))

;; Turn off the minor mode in the minibuffer
;; (defun turn-off-fhd-mode ()
;;   "Turn off fhd-mode."
;;   (fhd-mode -1))
;; (add-hook 'minibuffer-setup-hook #'turn-off-fhd-mode)

;; Minor mode tutorial: http://nullprogram.com/blog/2013/02/06/

(global-fhd-mode 1)
(bind-keys
 :map fhd-mode-map
 ("C-, C-j" . join-line)
 ("C-, n" . flymake-goto-next-error)
 ("M-g M-p" . flycheck-previous-error)
 ("M-g M-n" . flycheck-next-error)
 ("C-, C-s" . eshell)
 ("C-, p" . flymake-goto-prev-error)
 ("C-, C-t" . toggle-truncate-lines)
 ("C-, C-m" . bm-toggle)
 ("C-, C-p" . bm-previous)
 ("C-, C-z" . zap-up-to-char)
 ("C-, C-n" . bm-next))
