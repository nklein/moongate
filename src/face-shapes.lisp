;;; face-shapes.lisp

(in-package #:moongate)

(defun make-segment-face-path (theta/2 xoff yoff
                               &key
                                 (inner-radius *inner-radius*)
                                 (outer-radius *outer-radius*)
                                 (edge-material-thickness *edge-material-thickness*)
                                 (support-material-thickness *support-material-thickness*)
                                 (tabs-per-face-edge *tabs-per-face-edge*)
                                 (supports-per-piece *supports-per-piece*)
                                 (tabs-per-support-face *tabs-per-support-face*)
                                 (kerf *kerf*)
                               &allow-other-keys)
  (declare (ignore support-material-thickness
                   supports-per-piece
                   tabs-per-support-face))
  (let* ((outer-radius (+ outer-radius (/ kerf 2)))
         (inner-radius (- inner-radius (/ kerf 2)))
         (outer-kerf (atan (/ kerf 2) outer-radius))
         (inner-kerf (atan (/ kerf 2) inner-radius))
         (theta/2-outer (+ theta/2 outer-kerf))
         (theta/2-inner (+ theta/2 inner-kerf))
         (ss-outer (sin theta/2-outer))
         (cc-outer (cos theta/2-outer))
         (ss-inner (sin theta/2-inner))
         (cc-inner (cos theta/2-inner))
         (x1 (- (* inner-radius ss-inner) xoff))
         (y1 (- (* inner-radius cc-inner) yoff))
         (x2 (- (* outer-radius ss-outer) xoff))
         (y2 (- (* outer-radius cc-outer) yoff))
         (x4 (- x1))
         (y4 y1)
         (path (cl-svg:make-path)))
    (cl-svg:with-path path
      (cl-svg:move-to x1 y1)
      (cl-svg:line-to x2 y2))
    (make-outer-tabbed-curve path
                             theta/2
                             xoff
                             yoff
                             :outer-radius outer-radius
                             :tabs-per-face-edge tabs-per-face-edge
                             :edge-material-thickness edge-material-thickness
                             :kerf kerf)
    (cl-svg:with-path path
      (cl-svg:line-to x4 y4))
    (make-inner-tabbed-curve path
                             theta/2
                             xoff
                             yoff
                             :inner-radius inner-radius
                             :tabs-per-face-edge tabs-per-face-edge
                             :edge-material-thickness edge-material-thickness
                             :kerf kerf)
    (cl-svg:with-path path
      (cl-svg:close-path))
    path))

(defun make-support-tab-holes-in-face-path (phi xoff yoff
                                            &key
                                              (outer-radius *outer-radius*)
                                              (inner-radius *inner-radius*)
                                              (edge-material-thickness *edge-material-thickness*)
                                              (tabs-per-support-face *tabs-per-support-face*)
                                              (support-material-thickness *support-material-thickness*)
                                              (kerf *kerf*)
                                            &allow-other-keys)

  (let* ((length (- outer-radius inner-radius (* 2 edge-material-thickness)))
         (tab-distance-on-center (/ length tabs-per-support-face))
         (tab-length (/ tab-distance-on-center 2))
         (tab-length/2 (/ tab-length 2))
         (support-material-thickness/2 (/ support-material-thickness 2))
         (ss (sin phi))
         (cc (cos phi))
         (path (cl-svg:make-path)))
    (flet ((rotx (x y)
             (+ (* x (+ support-material-thickness/2 (/ kerf 2)) cc)
                (* y (+ tab-length/2 (/ kerf 2)) ss)))
           (roty (x y)
             (+ (* x (+ support-material-thickness/2 (/ kerf 2)) (- ss))
                (* y (+ tab-length/2 (/ kerf 2)) cc))))
      (loop :for k :from 0 :below tabs-per-support-face
            :for rr :from (+ inner-radius tab-length edge-material-thickness) :by tab-distance-on-center
            :do (let* ((cx (- (* rr ss) xoff))
                       (cy (- (* rr cc) yoff)))
                  (cl-svg:with-path path
                    (cl-svg:move-to (- cx (rotx -1 -1))
                                    (- cy (roty -1 -1)))
                    (cl-svg:line-to (- cx (rotx  1 -1))
                                    (- cy (roty  1 -1)))
                    (cl-svg:line-to (- cx (rotx  1  1))
                                    (- cy (roty  1  1)))
                    (cl-svg:line-to (- cx (rotx -1  1))
                                    (- cy (roty -1  1)))
                    (cl-svg:close-path)))))
    path))

(defun draw-segment-face (scene
                          &rest
                            args
                          &key
                            (outer-radius *outer-radius*)
                            (inner-radius *inner-radius*)
                            (kerf *kerf*)
                            (portion *portion*)
                            (pieces *pieces*)
                            (edge-material-thickness *edge-material-thickness*)
                            (cut-color *cut-color*)
                            (supports-per-piece *supports-per-piece*)
                            (stroke-width *stroke-width*)
                          &allow-other-keys)
  (let* ((theta (/ (* portion 2 pi)
                   pieces))
         (theta/2 (/ theta 2))
         (xoff 0)
         (yoff (/ (+ outer-radius
                     (* inner-radius (cos theta/2)))
                  2))
         (phi-increment (/ theta supports-per-piece))
         (phi-increment/2 (/ phi-increment 2)))
    (cl-svg:draw scene (:path :d (apply #'make-segment-face-path theta/2 xoff yoff args))
                 :fill "none"
                 :stroke cut-color
                 :stroke-width stroke-width)
    (loop :for phi :from (- theta/2 phi-increment/2) :by (- phi-increment)
          :repeat supports-per-piece
          :do (cl-svg:draw scene (:path :d (apply #'make-support-tab-holes-in-face-path
                                                  phi
                                                  xoff
                                                  yoff
                                                  :outer-radius outer-radius
                                                  :inner-radius inner-radius
                                                  :edge-material-thickness edge-material-thickness
                                                  args))
                           :fill "none"
                           :stroke cut-color
                           :stroke-width stroke-width))
    (+ (- outer-radius
          (* inner-radius (cos theta/2)))
       (* 2 kerf))))
