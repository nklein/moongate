;;; tabbed-lines.lisp

(in-package #:moongate)

(defun make-tabbed-line (path length
                         &key
                          (sx 0)
                          (sy 0)
                          (dx 1)
                          (dy 0)
                          (depth 1/8)
                          (tabs 3)
                          (kerf *kerf*))
  (labels ((rotx (x y)
             (+ (* x dx) (* y (- dy))))
           (roty (x y)
             (+ (* x dy) (* y dx)))
           (line-to-r (xx yy)
             (let ((x (rotx xx yy))
                   (y (roty xx yy)))
               (cl-svg:line-to-r x y))))
    (let* ((kerf/2 (/ kerf 2)) (tab-distance-on-center (/ length tabs))
           (tab-distance-on-center/4 (/ tab-distance-on-center 4)))
      (cl-svg:with-path path
        (cl-svg:line-to sx sy)
        (line-to-r kerf/2 0))
      (loop :for k :from 0 :below tabs
            :do (cl-svg:with-path path
                  (line-to-r (- tab-distance-on-center/4 kerf/2)
                             0)
                  (line-to-r 0
                             (- depth))
                  (line-to-r (+ (* 2 tab-distance-on-center/4) kerf)
                             0)
                  (line-to-r 0
                             depth)
                  (line-to-r (- tab-distance-on-center/4 kerf/2)
                             0)))
      (cl-svg:with-path path
        (line-to-r kerf/2 0))))
  path)
