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


(provide 'fhd-mode)
;; Minor mode tutorial: http://nullprogram.com/blog/2013/02/06/
