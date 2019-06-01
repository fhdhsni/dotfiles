(set-face-bold-p 'bold nil)
(defun fhd/set-helm-face ()
  "set helm face"
  (interactive)
  (progn
    (set-face-attribute 'helm-ff-directory nil :background nil :foreground "skyblue" :weight 'normal)
    (set-face-attribute 'helm-candidate-number nil :background nil)
    (set-face-attribute 'helm-selection nil :background nil :weight 'normal :foreground "PaleGreen")
    (set-face-attribute 'helm-source-header nil :background nil :foreground "Cyan1" :family "quicksand" :weight 'bold :height 170)))

(set-face-attribute 'variable-pitch nil :family "quicksand" :weight 'bold :height 1.1)
(set-face-attribute 'region nil :background "skyblue" :foreground "black")
(set-face-attribute 'mode-line-buffer-id nil :box nil :weight 'thin :height 1.0 :slant 'italic)
(set-face-attribute 'mode-line nil :background "white" :overline nil :box nil)

(with-eval-after-load "cus-edit"
  (set-face-attribute 'custom-face-tag nil :weight 'normal))
(with-eval-after-load "em-ls"
  (set-face-attribute 'eshell-ls-directory nil :weight 'normal))
(with-eval-after-load "em-prompt"
  (set-face-attribute 'eshell-prompt nil :weight 'normal))

(provide 'face-stuff)
