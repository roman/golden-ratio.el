;;; golden-ratio.el --- Automatic resizing of Emacs windows to the golden ratio

;; Copyright (C) 2012 Roman Gonzalez

;; Author: Roman Gonzalez <romanandreg@gmail.com>
;; Mantainer: Roman Gonzalez <romanandreg@gmail.com>
;; Created: 13 Oct 2012
;; Keywords: Window Resizing
;; Version: 0.0.4

;; Code inspired by ideas from Tatsuhiro Ujihisa

;; This file is not part of GNU Emacs.

;; This file is free software (MIT License)

;;; Code:
(eval-when-compile (require 'cl))

(defconst -golden-ratio-value 1.618
  "The golden ratio value itself.")

;; Major modes that are exempt from being resized. An example of this
;; for users of Org-mode might be:
;;  ("calendar-mode")
(defcustom golden-ratio-exclude-modes nil
  "An array of strings naming major modes. Switching to a buffer
whose major mode is a member of this list will not cause the
window to be resized to the golden ratio."
  :type '(repeat string)
  :group 'golden-ratio)

;; Buffer names that are exempt from being resized. An example of this
;; for users of Org-mode might be (note the leading spaces):
;;  (" *Org tags*" " *Org todo*")
(defcustom golden-ratio-exclude-buffer-names nil
  "An array of strings containing buffer names. Switching to a
buffer whose name is a member of this list will not cause the
window to be resized to the golden ratio."
  :type '(repeat string)
  :group 'golden-ratio)

(defun -golden-ratio-dimensions ()
  (let* ((main-rows     (floor (/ (frame-height) -golden-ratio-value)))
         (main-columns  (floor (/ (frame-width)  -golden-ratio-value))))
    (list main-rows
          main-columns)))


(defun -golden-ratio-resize-window (dimensions window)
  (let* ((edges           (window-pixel-edges window))
         (nrow            (floor
                           (- (first dimensions)
                              (window-height window))))
         (ncol            (floor
                           (- (second dimensions)
                              (window-width window)))))
    (progn
      (if (not (window-full-height-p))
          (enlarge-window nrow nil))
      (if (not (window-full-width-p))
          (enlarge-window ncol t)))))


;;;###autoload
(defun golden-ratio ()
  "Resizes current window to the golden-ratio's size specs"
  (interactive)
  (if (and (not (window-minibuffer-p))
           (not (one-window-p))
	   (not (member (symbol-name major-mode)
			golden-ratio-exclude-modes))
	   (not (member (buffer-name)
			golden-ratio-exclude-buffer-names)))
      (progn
        (balance-windows)
        (-golden-ratio-resize-window (-golden-ratio-dimensions)
                                     (selected-window)))))


(defadvice select-window
  (after golden-ratio-resize-window)
  (golden-ratio))

(defadvice other-window
  (after golden-ratio-resize-window)
  (golden-ratio))

(defadvice split-window
  (after golden-ratio-resize-window)
  (golden-ratio))


;;;###autoload
(defun golden-ratio-enable ()
  "Enables golden-ratio's automatic window resizing"
  (interactive)
  (ad-activate 'select-window)
  (ad-activate 'other-window)
  (ad-activate 'split-window))


;;;###autoload
(defun golden-ratio-disable ()
  "Disables golden-ratio's automatic window resizing"
  (interactive)
  (ad-deactivate 'select-window)
  (ad-deactivate 'other-window)
  (ad-deactivate 'split-window))


(provide 'golden-ratio)

;;; golden-ratio.el ends here
