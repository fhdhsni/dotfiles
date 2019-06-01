
;;; pixel-scroll.el --- Scroll a line smoothly

;; Package-Requires: ((emacs "24.5"))
;; Version: 1.0.0
;; Package-Version: 20170415.0722
;; Keywords: convenience, usability

;;; This file is NOT part of GNU Emacs

;;; License

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; To interactively toggle the mode on / off:
;;
;;   M-x pixel-scroll-mode
;;
;; To make the mode permanent, put this in your init file:
;;
;;   (require 'pixel-scroll)
;;   (pixel-scroll-mode 1)
;;
;; This package offers a global minor mode which makes Emacs scroll
;; vertically with feel of modern applications.  This minor mode
;; offers pixel-by-pixel scroll upward by mouse wheel using
;; `set-window-vscroll', `window-vscroll', and `scroll-up'.  The minor
;; mode overwrites parameters defined in `mwheel.el' to refer
;; `pixel-scroll-up' and `pixel-scroll-down' instead of `scroll-up'
;; and `scroll-down'.

;;; Principle of vertical scroll:

;; Scrolling text upward a line by pixels using `set-window-vscroll'
;; and by a line using `scroll-up' gives similar visual feedback when
;; vscroll location is @0.  Note vscroll location is vertical shift
;; obtained by `window-vscroll'.  Line height by pixel is obtained by
;; `frame-char-height' (to be exact, this is true for buffer with
;; mono-sized font).  Following two lines scroll text in similar
;; fashion, visually.
;;
;;   (scroll-up 1)
;;   (set-window-vscroll nil (frame-char-height) t)
;;
;; Scrolling text upward by a pixel and a line yields similar result
;; when vscroll location is at the last pixel.  Following two lines
;; scroll text in similar fashion, visually.
;;
;;   (scroll-up 1)
;;   (set-window-vscroll nil (1- (frame-char-height) t)) (scroll-up 1)
;;
;; When vscroll gets larger and as soon as point is beyond beginning
;; of a window, vscroll is set to zero.  To user, scope is changed
;; suddenly without point moved.  This package tries to scroll text
;; upward by a line with pixel-by-pixel transition by following
;; sequences.
;;
;;   (progn
;;     (vertical-motion 1)
;;     (dolist (vs (number-sequence 1 (1- (frame-char-height))))
;;       (set-window-vscroll nil vs t) (sit-for 0.001))
;;     (scroll-up 1))

;;; Remarks:

;; If you want to try pixel-level scrolling, that is in the middle of
;; implementation, also include following in your init file:
;;
;; (setq pixel-resolution-fine-p nil)

;;; Change Log:

;; 20170319.1153
;;  - Replace `frame-char-height' by `line-pixel-height'.
;; 20170414.0958
;;  - Algorithm to scroll-down is offered by Eli Zaretskii.
;;  - Implement scroll pixel-by-pixel upward.

;;; Todo:
;; - Estimate height of unseen line at the top, on scrolling down.
;; - Handle error to scroll stable For now, Scroll does not well in Info.

;;; Long term concern:
;; - After a pixel scroll, typing C-n or C-p scrolls the window to make
;;   it fully visible, and undos the effect of the pixel-level scroll.

;;; Code:

(require 'mwheel)

(defgroup pixel-scroll nil
  "Scroll pixel-by-pixel in Emacs."
  :group 'mouse
  :prefix "pixel-")

(defcustom pixel-wait 0.001
  "Idle time on pixel scroll specified in second."
  :group 'pixel-scroll
  :type 'float)

(defcustom pixel-amount '(1 ((shift) . 5) ((control)))
  "Amount to scroll by when spinning the mouse wheel."
  :group 'pixel-scroll)

(defcustom pixel-resolution-fine-p nil
  "Enhance scrolling resolution to pixel-to-pixel instead of
line-to-line."
  :group 'pixel-scroll
  :type 'boolean)

(define-minor-mode pixel-scroll-mode
  "A minor mode to scroll text pixel-by-pixel.  With a prefix argument ARG,
enable Pixel Scroll mode if ARG is positive, and disable it
otherwise.  If called from Lisp, enable Pixel Scroll mode if ARG
is omitted or nil."
  :init-value nil
  :group 'pixel-scroll
  :global t

  (if pixel-scroll-mode
      (progn (setq mwheel-scroll-up-function 'pixel-scroll-up)
             (setq mwheel-scroll-down-function 'pixel-scroll-down)
             (setq mouse-wheel-scroll-amount pixel-amount)
             (setq mouse-wheel-progressive-speed pixel-resolution-fine-p))
    (setq mwheel-scroll-up-function 'scroll-up)
    (setq mwheel-scroll-down-function 'scroll-down)
    (dolist (var '(mouse-wheel-scroll-amount
                   mouse-wheel-progressive-speed))
      (custom-reevaluate-setting var))))

(defun pixel-scroll-up (&optional arg)
  "Scroll text of selected window up ARG lines.  This is
alternative of `scroll-up'.  Scope moves downward."
  (interactive)
  (or arg (setq arg 1))
  (dotimes (ii arg) ; move scope downward
    (if (<= (count-lines (window-start) (window-end)) 2)
        (scroll-up 1) ; when end of scroll is close, relay on robust guy
      (when (or (pixel-point-at-top-p) ; prevent too late
                (and scroll-preserve-screen-position
                     (not (pixel-point-at-bottom-p)))) ; prevent too fast
        (vertical-motion 1)) ; move point downward
      (pixel-scroll-pixel-up (if pixel-resolution-fine-p
                                 1
                               (pixel-line-height)))))) ; move scope downward

(defun pixel-scroll-down (&optional arg)
  "Scroll text of selected window down ARG lines.  This is
alternative of `scroll-down'.  Scope moves upward."
  (interactive)
  (or arg (setq arg 1))
  (dotimes (ii arg)
    (if (equal (window-start) (point-min))
        (scroll-down 1) ; when beginning-of-buffer is seen, relay on robust guy
      (when (or (pixel-point-at-bottom-p) ; prevent too late
                (and scroll-preserve-screen-position
                     (not (pixel-point-at-top-p)))) ; prevent too fast
        (vertical-motion -2))) ; FIXME: -1 gives glitch
    (pixel-scroll-pixel-down (if pixel-resolution-fine-p
                                 1
                               (pixel-line-height)))))

(defun pixel-point-at-top-p ()
  "Return if point is at top of a window."
  (equal (save-excursion (beginning-of-visual-line)
                         (point-at-bol))
         (window-start)))

(defun pixel-point-at-bottom-p ()
  "Return if point is at bottom of a window."
  (<= (count-lines (save-excursion
                     (beginning-of-visual-line)
                     (point-at-bol))
                   (pixel-window-end)) 1))

(defun pixel-scroll-pixel-up (amt)
  "Scroll text of selected windows up AMT pixels.  Scope moves
downward."
  (while (>= (+ (window-vscroll nil t) amt)
             (pixel-line-height))
    (setq amt (- amt (pixel--catch-line-up)))) ; major scroll
  (pixel--sweep-pixel-up amt)) ; minor scroll

(defun pixel-scroll-pixel-down (amt)
  "Scroll text of selected windows down AMT pixels.  Scope moves
upward."
  ;; FIXME: Cannot scroll down on Info sometimes
  (while (> amt 0)
    (let ((vs (window-vscroll nil t)))
      (if (equal vs 0)
          (pixel-scroll-down-and-set-window-vscroll (1- (pixel-line-height)))
        (set-window-vscroll nil (1- vs) t))
      (setq amt (1- amt))
      (sit-for pixel-wait))))

(defun pixel--catch-line-up ()
  "Flush text upward a line with pixel transition.  When `vscroll' is non-zero,
complete scrolling a line.  When `vscroll' is larger than height
of multiple lines, for example 88, this flushes multiple lines.
At the end, `vscroll' will be zero.  This assumes that the lines
are with the same height.  Scope moves downward.  This function
returns number of pixels that were scrolled."
  (let* ((src (window-vscroll nil t))  ; EXAMPLE (initial)      @0   @8  @88
         (height (pixel-line-height))  ;                        25   25   23
         (line (1+ (/ src height)))    ; catch up + one line    Δ1   Δ1   Δ4
         (dst (* line height))         ; goal                  @25  @25  @92
         (delta (- dst src)))          ; pixels to be scrolled  25   17    4
    (pixel--sweep-pixel-up (1- delta)) ; sweep until one less  @24  @24  @91
    (scroll-up line) (sit-for pixel-wait) ; scroll 1 pixel      @0   @0   @0
    delta))

(defun pixel--sweep-pixel-up (n)
  "Sweep text upward to N pixels.  Scope moves downward."
  (when (> n 0)
    (let ((vs0 (window-vscroll nil t)))
      (dolist (vs (number-sequence (1+ vs0) (+ vs0 n)))
        (set-window-vscroll nil vs t) (sit-for pixel-wait)))))

(defun pixel-window-end ()
  "Return position of the last character of fully-visible line in
WINDOW.  This is similar to `window-end' but see a full visible
line."
  (let ((pos (window-end)))
    (if (pos-visible-in-window-p pos nil t)
        pos
      (save-excursion
        (goto-char pos)
        (vertical-motion -2)
        (point-at-bol)))))

(defun pixel-line-height (&optional pos)
  "Measure line height of POS in pixel.  When height of all lines
are equal, you don't need this function but `frame-char-height'.
See Info node `(elisp) Line Height'."
  (or pos (setq pos (window-start)))
  (save-excursion
    (goto-char pos)
    (line-pixel-height)))

(defun pixel-scroll-down-and-set-window-vscroll (vscroll)
  "Scroll down a line and set VSCROLL in pixel.  This is written
by Eli Zaretskii.  It is important to call `set-window-start' to
force the display engine use that particular position as the
window-start point.  Otherwise, redisplay will reset the window's
vscroll."
  (let ((pos
         (save-excursion
           (goto-char (window-start))
           (beginning-of-visual-line 0))))
    (set-window-start nil pos t)
    (set-window-vscroll nil vscroll t)))

(provide 'pixel-scroll)
;;; pixel-scroll.el ends here
