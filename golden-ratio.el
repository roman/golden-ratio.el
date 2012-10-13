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
  (interactive)
  (if (not (window-minibuffer-p))
      (progn
        (balance-windows)
        (-golden-ratio-resize-window (-golden-ratio-dimensions)
                                     (selected-window)))))

(defadvice select-window
  (after golden-ratio-resize-window)
  (golden-ratio))

(defun golden-ratio-enable ()
  (interactive)
  (ad-activate 'select-window))

(defun golden-ratio-disable ()
  (interactive)
  (ad-deactivate 'select-window))

(provide 'golden-ratio)
