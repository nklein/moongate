;;; tabbed-curves.lisp

(in-package #:moongate)

(defun make-outer-tabbed-curve (path theta/2 xoff yoff
                                 &key
                                 (outer-radius *outer-radius*)
                                 (tabs-per-face-edge *tabs-per-face-edge*)
                                 (edge-material-thickness *edge-material-thickness*)
                                 (kerf *kerf*))
  (let* ((inner-radius (- outer-radius edge-material-thickness))
         (outer-kerf (atan (/ kerf 2) outer-radius))
         (inner-kerf (atan (/ kerf 2) inner-radius))
         (phi-increment (/ theta/2 tabs-per-face-edge 1/2))
         (phi-increment/4 (/ phi-increment 4)))

    (loop :for phi :from theta/2 :by (- phi-increment)
          :for k :from 0 :below tabs-per-face-edge
          :do (let* ((phi1 phi)
                     (phi2 (- phi phi-increment/4))
                     (phi3 (- phi (* 3 phi-increment/4)))
                     (phi4 (- phi phi-increment))
                     (phi1o (+ phi1 outer-kerf))
                     (phi2o (- phi2 outer-kerf))
                     (phi2i (- phi2 inner-kerf))
                     (phi3i (+ phi3 inner-kerf))
                     (phi3o (+ phi3 outer-kerf))
                     (phi4o (- phi4 outer-kerf)))
                (when (zerop k)
                  (cl-svg:with-path path
                    (cl-svg:line-to (- (* outer-radius (sin phi1o)) xoff)
                                    (- (* outer-radius (cos phi1o)) yoff))))
                (cl-svg:with-path path
                  (cl-svg:arc-to outer-radius outer-radius 0 0 1
                                 (- (* outer-radius (sin phi2o)) xoff)
                                 (- (* outer-radius (cos phi2o)) yoff))
                  (cl-svg:line-to (- (* inner-radius (sin phi2i)) xoff)
                                  (- (* inner-radius (cos phi2i)) yoff))
                  (cl-svg:arc-to inner-radius inner-radius 0 0 1
                                 (- (* inner-radius (sin phi3i)) xoff)
                                 (- (* inner-radius (cos phi3i)) yoff))
                  (cl-svg:line-to (- (* outer-radius (sin phi3o)) xoff)
                                  (- (* outer-radius (cos phi3o)) yoff))
                  (cl-svg:arc-to outer-radius outer-radius 0 0 1
                                 (- (* outer-radius (sin phi4o)) xoff)
                                 (- (* outer-radius (cos phi4o)) yoff)))))))

(defun make-inner-tabbed-curve (path theta/2 xoff yoff
                                 &key
                                 (inner-radius *inner-radius*)
                                 (tabs-per-face-edge *tabs-per-face-edge*)
                                 (edge-material-thickness *edge-material-thickness*)
                                 (kerf *kerf*))
  (let* ((outer-radius (+ inner-radius edge-material-thickness))
         (outer-kerf (atan (/ kerf 2) outer-radius))
         (inner-kerf (atan (/ kerf 2) inner-radius))
         (phi-increment (/ theta/2 tabs-per-face-edge 1/2))
         (phi-increment/4 (/ phi-increment 4)))

    (loop :for phi :from (- theta/2) :by phi-increment
          :for k :from 0 :below tabs-per-face-edge
          :do (let* ((phi1 phi)
                     (phi2 (+ phi phi-increment/4))
                     (phi3 (+ phi (* 3 phi-increment/4)))
                     (phi4 (+ phi phi-increment))
                     (phi1i (- phi1 inner-kerf))
                     (phi2i (+ phi2 inner-kerf))
                     (phi2o (+ phi2 outer-kerf))
                     (phi3o (- phi3 outer-kerf))
                     (phi3i (- phi3 inner-kerf))
                     (phi4i (+ phi4 inner-kerf)))
                (when (zerop k)
                  (cl-svg:with-path path
                    (cl-svg:line-to (- (* inner-radius (sin phi1i)) xoff)
                                    (- (* inner-radius (cos phi1i)) yoff))))
                (cl-svg:with-path path
                  (cl-svg:arc-to inner-radius inner-radius 0 0 1
                                 (- (* inner-radius (sin phi2i)) xoff)
                                 (- (* inner-radius (cos phi2i)) yoff))
                  (cl-svg:line-to (- (* outer-radius (sin phi2o)) xoff)
                                  (- (* outer-radius (cos phi2o)) yoff))
                  (cl-svg:arc-to outer-radius outer-radius 0 0 1
                                 (- (* outer-radius (sin phi3o)) xoff)
                                 (- (* outer-radius (cos phi3o)) yoff))
                  (cl-svg:line-to (- (* inner-radius (sin phi3i)) xoff)
                                  (- (* inner-radius (cos phi3i)) yoff))
                  (cl-svg:arc-to inner-radius inner-radius 0 0 1
                                 (- (* inner-radius (sin phi4i)) xoff)
                                 (- (* inner-radius (cos phi4i)) yoff)))))))
