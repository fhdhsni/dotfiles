;; (use-package minibuffer-line
;;   :init
;;   (require 'cal-persia)
;;   (require 'calendar)
;;   (defun fhd/used-ram ()
;;     "calculate ram usage"
;;     (interactive)
;;     (let ((all (loop for n in (memory-info)
;;                      collect (/ n 1024))))
;;       (let* ((total (car all))
;;              (used (car (cdr all))))
;;         (- total used))))

;;   (defface fhd/minibuffer-line-face
;;     '((t (:height .8)))
;;     "face of minibuffer-line")

;;   (setq minibuffer-line-format
;;         '(""
;;           " "
;;           (:eval (propertize
;;                   (let ((today (calendar-day-of-week (calendar-current-date))))
;;                     (cond
;;                      ((eq today 6) "Saturday")
;;                      ((eq today 0) "Sunday")
;;                      ((eq today 1) "Monday")
;;                      ((eq today 2) "Tuesday")
;;                      ((eq today 3) "Wednesday")
;;                      ((eq today 4) "Thursday")
;;                      ((eq today 5) "Friday"))) 'face 'fhd/minibuffer-line-face))
;;           " | "
;;           (:eval
;;            (propertize (format-time-string "%F %R") 'face 'fhd/minibuffer-line-face))
;;           " | "
;;           (:eval (propertize (format "%s" (calendar-persian-date-string)) 'face 'fhd/minibuffer-line-face))
;;           " | "
;;           (:eval (propertize (format "%s MB" (fhd/used-ram)) 'face 'fhd/minibuffer-line-face))))
;;   (minibuffer-line-mode))
