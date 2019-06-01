(defun mode-line-fill-right (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun mode-line-fill-center (face reserve)
  "Return empty space using FACE to the center of remaining space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
                                             (.5 . left-margin))))
              'face face))

(defconst RIGHT_PADDING 2)

(defun reserve-left/middle ()
  (/ (length (format-mode-line mode-line-align-middle)) 2))

(defun reserve-middle/right ()
  (+ RIGHT_PADDING (length (format-mode-line mode-line-align-right))))

(setq mode-line-align-left
      '(""
        "%1 "
        ;; (:eval (format "%s" exwm-workspace-current-index))
        ;; "%1 "
        ;; (:eval
        ;;  (if (and exwm-input-line-mode-passthrough (string= major-mode "exwm-mode"))
        ;;      (format "%s" "Emacs")))
        "%2 "
        (:propertize "%b" face mode-line-buffer-id)
        "%1 "
        (:eval (when (bound-and-true-p flycheck-mode)  (flycheck-mode-line-status-text)))
        " "))

(setq mode-line-align-middle
      '(""
        (vc-mode vc-mode)
        "%1 "
        ;; (:propertize "%b" face mode-line-buffer-id)
        (:eval
         (when (eql (buffer-modified-p) t)
           (propertize "⚑" 'face '(:foreground "darkmagenta" :height 1.0))))
        " "
        (:eval
         (when (eql override-global-mode nil)
           (propertize "⏺" 'face '(:foreground "darkmagenta"))))
        " "
        (:eval
         (when (eql buffer-read-only t)
           ;; (propertize "" 'face '(:foreground "#6c6c6c"))))
           (propertize "Read-Only" 'face '(:foreground "#3a3a3a" :height .9))))
        ""))

;; (setq mode-line-align-right
;;       '(""
;;         mode-line-misc-info
;;         "%2 "
;;         (:eval (format "%%l/%d : %%c " (line-number-at-pos (point-max))))))

(setq mode-line-align-right
      '(""
        ;; mode-line-misc-info
        "%1 "
        (:eval (format "%%l/%d:%%c " (line-number-at-pos (point-max))))))

(setq fhd/mode-line-fromat (list
               mode-line-align-left
               '(:eval (mode-line-fill-center 'nil
                                              (reserve-left/middle)))
               mode-line-align-middle
               '(:eval
                 (mode-line-fill-right 'nil
                                       (reserve-middle/right)))
               mode-line-align-right))

(defun fhd/toggle-mode-line ()
  (interactive)
  (cond ((eq mode-line-format nil)
         (setq-default mode-line-format fhd/mode-line-fromat))
        (t (setq-default mode-line-format nil))))

(defun fhd/nil-mode-line ()
  (interactive)
  (setq-default mode-line-format nil))

(defun fhd/show-mode-line ()
  (interactive)
  (setq-default mode-line-format fhd/mode-line-fromat))
