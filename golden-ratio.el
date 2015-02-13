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

(defconst golden-ratio--value 1.618
  "The golden ratio value itself.")

(defgroup golden-ratio nil
  "Resize windows to golden ratio."
  :group 'windows)

;; Major modes that are exempt from being resized. An example of this
;; for users of Org-mode might be:
;;  ("calendar-mode")
(defcustom golden-ratio-exclude-modes nil
  "An array of strings naming major modes.
Switching to a buffer whose major mode is a member of this list
will not cause the window to be resized to the golden ratio."
  :type '(repeat string)
  :group 'golden-ratio)

;; Buffer names that are exempt from being resized. An example of this
;; for users of Org-mode might be (note the leading spaces):
;;  (" *Org tags*" " *Org todo*")
(defcustom golden-ratio-exclude-buffer-names nil
  "An array of strings containing buffer names.
Switching to a buffer whose name is a member of this list
will not cause the window to be resized to the golden ratio."
  :type '(repeat string)
  :group 'golden-ratio)

(defcustom golden-ratio-inhibit-functions nil
  "List of functions to call with no arguments.
Switching to a buffer, if any of these functions returns non-nil
will not cause the window to be resized to the golden ratio."
  :group 'golden-ratio
  :type '(repeat symbol))

(defcustom golden-ratio-extra-commands
  '(windmove-left windmove-right windmove-down windmove-up)
  "List of extra commands used to jump to other window."
  :group 'golden-ratio
  :type '(repeat symbol))

(defcustom golden-ratio-recenter nil
  "Recenter window vertically and scroll right when non--nil."
  :group 'golden-ratio
  :type 'boolean)

(defcustom golden-ratio-adjust-width 1.0
  "Adjust the width sizing by some factor. 1 is no adjustment.
   For very wide screens/frames .4 may work well."
  :group 'golden-ratio
  :type 'integer)

;;; Compatibility
;;
(unless (fboundp 'window-resizable-p)
  (defalias 'window-resizable-p 'window--resizable-p))

(defun golden-ratio-adjust (a)
  "set the adjustment of window widths."
  (interactive
   (list
    (read-number "Adjust: " golden-ratio-adjust-width)))
  (setq golden-ratio-adjust-width a))

(defun golden-ratio--dimensions ()
  (list (floor (/ (frame-height) golden-ratio--value))
        (floor  (* (/ (frame-width)  golden-ratio--value)
                   golden-ratio-adjust-width))))

(defun golden-ratio--resize-window (dimensions &optional window)
  (with-selected-window (or window (selected-window))
    (let ((nrow  (floor (- (first  dimensions) (window-height))))
          (ncol  (floor (- (second dimensions) (window-width)))))
      (when (window-resizable-p (selected-window) nrow)
        (enlarge-window nrow))
      (when (window-resizable-p (selected-window) ncol t)
        (enlarge-window ncol t)))))

;;;###autoload
(defun golden-ratio ()
  "Resizes current window to the golden-ratio's size specs."
  (interactive)
  (unless (or (window-minibuffer-p)
              (one-window-p)
              (member (symbol-name major-mode)
                      golden-ratio-exclude-modes)
              (member (buffer-name)
                      golden-ratio-exclude-buffer-names)
              (and golden-ratio-inhibit-functions
                   (loop for fun in golden-ratio-inhibit-functions
                         thereis (funcall fun))))
    (let ((dims (golden-ratio--dimensions))
          (golden-p (if golden-ratio-mode 1 -1)))
      ;; Always disable `golden-ratio-mode' to avoid
      ;; infinite loop in `balance-windows'.
      (golden-ratio-mode -1)
      (balance-windows)
      (golden-ratio--resize-window dims)
      (when golden-ratio-recenter
        (scroll-right) (recenter))
      (golden-ratio-mode golden-p))))

;; Should return nil
(defadvice other-window
    (after golden-ratio-resize-window)
  (golden-ratio) nil)

;; Should return the buffer
(defadvice pop-to-buffer
    (around golden-ratio-resize-window)
  (prog1 ad-do-it (golden-ratio)))

(defun golden-ratio--post-command-hook ()
  (when (or (memq this-command golden-ratio-extra-commands)
            (and (consp this-command) ; A lambda form.
                 (loop for com in golden-ratio-extra-commands
                       thereis (or (memq com this-command)
                                   (memq (car-safe com) this-command)))))
    ;; This is needed in emacs-25 to avoid this error from `recenter':
    ;; `recenter'ing a window that does not display current-buffer.
    ;; This doesn't happen in emacs-24.4 and previous versions.
    (run-with-idle-timer 0.01 nil (lambda () (golden-ratio)))))

(defun golden-ratio--mouse-leave-buffer-hook ()
  (run-at-time 0.1 nil (lambda ()
			 (golden-ratio))))

;;;###autoload
(define-minor-mode golden-ratio-mode
    "Enable automatic window resizing with golden ratio."
  :lighter " Golden"
  :global t
  (if golden-ratio-mode
      (progn
        (add-hook 'window-configuration-change-hook 'golden-ratio)
        (add-hook 'post-command-hook 'golden-ratio--post-command-hook)
        (add-hook 'mouse-leave-buffer-hook 'golden-ratio--mouse-leave-buffer-hook)
        (ad-activate 'other-window)
        (ad-activate 'pop-to-buffer))
      (remove-hook 'window-configuration-change-hook 'golden-ratio)
      (remove-hook 'post-command-hook 'golden-ratio--post-command-hook)
      (remove-hook 'mouse-leave-buffer-hook 'golden-ratio--mouse-leave-buffer-hook)
      (ad-deactivate 'other-window)
      (ad-deactivate 'pop-to-buffer)))


(provide 'golden-ratio)

;;; golden-ratio.el ends here
