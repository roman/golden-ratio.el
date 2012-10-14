;;; golden-ratio.el --- Automatic resizing of Emacs windows to the golden ratio

;; Copyright (C) 2012 Roman Gonzalez

;; Author: Roman Gonzalez <romanandreg@gmail.com>
;; Mantainer: Roman Gonzalez <romanandreg@gmail.com>
;; Created: 13 Oct 2012
;; Keywords: Window Resizing
;; Version: 0.0.2

;; This file is not part of GNU Emacs.

;; This file is free software (MIT License)

;;; Code:

(defun -golden-ratio-dimensions ()
  (let* ((main-rows     (floor (/ (frame-height) 1.618)))
         (main-columns  (floor (/ (frame-width)  1.618))))
    (list main-rows
          main-columns)))


(defun -golden-ratio-resize-window (dimensions window)
  (let* ((edges           (window-absolute-pixel-edges window))
         (nrow            (floor
                           (- (first dimensions)
                              (window-height window))))
         (ncol            (floor
                           (- (second dimensions)
                              (window-width window)))))
    (progn
      (enlarge-window nrow nil)
      (enlarge-window ncol t))))


(defun golden-ratio ()
  "Resizes current window to the golden-ratio's size specs"
  (interactive)
  (if (and (not (window-minibuffer-p))
           (not (one-window-p)))
      (progn
        (balance-windows)
        (-golden-ratio-resize-window (-golden-ratio-dimensions)
                                     (selected-window)))))


(defadvice select-window
  (after golden-ratio-resize-window)
  (golden-ratio))


(defun golden-ratio-enable ()
  "Enables golden-ratio's automatic window resizing"
  (interactive)
  (ad-activate 'select-window))


(defun golden-ratio-disable ()
  "Disables golden-ratio's automatic window resizing"
  (interactive)
  (ad-deactivate 'select-window))


(provide 'golden-ratio)

;;; filename ends here
