;;; helpers.el --- some helpers functions

;;; Commentary:

;;; Code:

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
                           (start-process "" nil "xdg-open" -fpath))) -file-list))))

;; (defun fhd/open-in-terminal ()
;;   "Open the current dir in a new terminal window."
;;   (interactive)
;;   (let ((process-connection-type nil))
;;     (exwm-workspace-switch 6)
;;     ;; (start-process "" nil "termite"))
;;     ;; (start-process "" nil "gnome-terminal")
;;     (start-process "" nil "alacritty")
;;     (start-process "" nil "konsole")))

;; (defvar helm-fzf-source
;;   (helm-build-async-source "fzf"
;;     :candidates-process 'helm-fzf--do-candidate-process
;;     :nohighlight t
;;     :requires-pattern 2
;;     :action 'helm-type-file-actions
;;     :candidate-number-limit 9999))

;; (defun helm-fzf--do-candidate-process ()
;;   (let* ((cmd-args `("fzf" "-x" "-f" ,helm-pattern))
;;          (proc (apply #'start-file-process "helm-fzf" nil cmd-args)))
;;     (prog1 proc
;;       (set-process-sentinel
;;        proc
;;        (lambda (process event)
;;          (helm-process-deferred-sentinel-hook
;;           process event (helm-default-directory)))))))

;; (defun helm-fzf ()
;;   (interactive)
;;   (let ((default-directory default-directory))
;;     (helm :sources '(helm-fzf-source)
;;           :buffer "*helm-fzf*")))

;; (defun fhd/helm-locate-current-directory ()
;;   (interactive)
;;   (if (equal default-directory "~/")
;;       (helm-locate-with-db nil "/home/farhad/ ")
;;     (helm-locate-with-db nil (concat default-directory " "))))

(defun fhd/switch-to-last-window ()
  "Switch to previous window."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

(defun fhd/open-adata ()
  "Open my external HDD."
  (interactive)
  (dired "/run/media/farhad/adata/"))

(defun fhd/open-home ()
  "Open my ~."
  (interactive)
  (dired "~/"))

(defun fhd/find-file-recursivly (str)
  "Recursively find files containing STR with `directory-files-recursively'."
  (interactive "sGimme a Pattern: ")
  (ivy-read "File: " (directory-files-recursively "." str t)))

(defun fhd/open-init ()
  "Open init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; (face-remap-add-relative 'default :family "Source Code Pro" :height 140)

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

(defun fhd/set-backlight (percent)
  "Set dispaly backlight to PERCENT."
  (interactive "sHow much brightness? ")
  (start-process-shell-command "setbacklight" nil (concat "xbacklight -set " percent)))

(defun fhd/set-volume (volume)
  "Set VOLUME."
  (interactive "sWhat percentage of volume? ")
  (let ((command (concat  "amixer set Master " (concat volume "%"))))
    (start-process-shell-command "amixer" nil command)))

;; (defun fhd/rofi ()
;;   "run rofi launcher"
;;   (interactive)
;;   (start-process-shell-command "rofirun" nil "rofi -show run"))

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

(defun fhd/duckduckgo-web (term)
  "Search in the web using `browse-url'."
  (interactive "MSearch on the web? ")
  (browse-url (concat "https://duckduckgo.com/?q="
                      (url-hexify-string term))))

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


(provide 'helpers)

;; Local Variables:
;; byte-compile: t
;; End:

;;; helpers.el ends here
