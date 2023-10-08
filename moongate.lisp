(ql:quickload :cl-svg)

(defparameter *inner-radius* 86
  "Inner radius of gate (in inches)")
(defparameter *outer-radius* 96
  "Outer radius of gate (in inches)")
(defparameter *support-height* 10
  "Seperation between front of ring and back of ring (in inches)")

(defparameter *portion* 2/3
  "Portion of the circle we are constructing")
(defparameter *pieces* 9
  "Number of pieces we are breaking that portion of the circle into")

(defparameter *kerf* 1/10
  "Width of cutting tool (in inches)")

(defparameter *stroke-width* 1/1000
  "Width of strokes in drawing (in inches)")

(defparameter *sheet-width* 48)
(defparameter *sheet-height* 24)
(defparameter *dpi* 96
  "Dots per inch (inkscape assumes 96)")

(defparameter *cut-color* "red"
  "Color to mark the lines to be cut")
(defparameter *mark-color* "black"
  "Color to mark the lines to be engraved")

(defparameter *tabs-per-face-edge* 10
  "Number of tabs holding edge pieces to face pieces")
(defparameter *supports-per-piece* 5
  "Number of supports holding each front face away from the matching back face")
(defparameter *tabs-per-support-edge* 4
  "Number of tabs the supports use to stick into the edge pieces")
(defparameter *tabs-per-support-face* 4
  "Number of tabs the supports use to stick into the face pieces")

(defparameter *edge-material-thickness* 1/8
  "Thickness of material bent around the edges (in inches)")
(defparameter *support-material-thickness* 1/8
  "Thickness of material used for support pieces (in inches)")
(defparameter *face-material-thickness* 1/8
  "Thickness of material used for the face pieces (in inches)")

(defun make-outer-tabbed-curve (path theta/2 xoff yoff
                                 &key
                                 (outer-radius *outer-radius*)
                                 (tabs-per-face-edge *tabs-per-face-edge*)
                                 (edge-material-thickness *edge-material-thickness*)
                                 (kerf *kerf*))
  (let* ((inner-radius (- outer-radius edge-material-thickness))
         (outer-kerf (atan kerf outer-radius))
         (inner-kerf (atan kerf inner-radius))
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
         (outer-kerf (atan kerf outer-radius))
         (inner-kerf (atan kerf inner-radius))
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
  (let* ((outer-radius (+ outer-radius kerf))
         (inner-radius (- inner-radius kerf))
         (outer-kerf (atan kerf outer-radius))
         (inner-kerf (atan kerf inner-radius))
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
                                              (tabs-per-support-face *tabs-per-support-face*)
                                              (support-material-thickness *support-material-thickness*)
                                              (kerf *kerf*)
                                            &allow-other-keys)

  (let* ((length (- outer-radius inner-radius))
         (tab-distance-on-center (/ length tabs-per-support-face))
         (tab-length (/ tab-distance-on-center 2))
         (tab-length/2 (/ tab-length 2))
         (support-material-thickness/2 (/ support-material-thickness 2))
         (ss (sin phi))
         (cc (cos phi))
         (path (cl-svg:make-path)))
    (flet ((rotx (x y)
             (+ (* x (+ support-material-thickness/2 kerf) cc)
                (* y (+ tab-length/2 kerf) ss)))
           (roty (x y)
             (+ (* x (+ support-material-thickness/2 kerf) (- ss))
                (* y (+ tab-length/2 kerf) cc))))
      (loop :for k :from 0 :below tabs-per-support-face
            :for rr :from (+ inner-radius tab-length) :by tab-distance-on-center
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
                            (portion *portion*)
                            (pieces *pieces*)
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
                                                  args))
                           :fill "none"
                           :stroke cut-color
                           :stroke-width stroke-width))
    scene))

(defun draw-moongate (filename
                      &rest
                        args
                      &key
                        (inner-radius *inner-radius*)
                        (outer-radius *outer-radius*)
                        (portion *portion*)
                        (pieces *pieces*)
                        (kerf *kerf*)
                        (cut-color *cut-color*)
                        (mark-color *mark-color*)
                        (stroke-width *stroke-width*)
                        (sheet-width *sheet-width*)
                        (sheet-height *sheet-height*)
                        (dpi *dpi*)
                        (edge-material-thickness *edge-material-thickness*)
                        (face-material-thickness *face-material-thickness*)
                        (support-material-thickness *support-material-thickness*)
                        (tabs-per-face-edge *tabs-per-face-edge*)
                        (supports-per-piece *supports-per-piece*)
                        (tabs-per-support-face *tabs-per-support-face*)
                        (tabs-per-support-edge *tabs-per-support-edge*))
  (declare (ignorable inner-radius
                      outer-radius
                      portion
                      pieces
                      kerf
                      cut-color
                      mark-color
                      stroke-width
                      dpi
                      edge-material-thickness
                      face-material-thickness
                      support-material-thickness
                      tabs-per-face-edge
                      supports-per-piece
                      tabs-per-support-face
                      tabs-per-support-edge))
  (let* ((scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                          :width (* sheet-width dpi)
                                          :height (* sheet-height dpi))))
    (cl-svg:title scene "moongate")
    (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) (- (- (* sheet-width 1/8)) 1/2)))
      (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
        (let ((group (cl-svg:make-group scene ())))
          (apply #'draw-segment-face group :kerf 0 :cut-color mark-color :stroke-width (/ stroke-width 2) args)
          (apply #'draw-segment-face group args))))
    (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) (- (- (* sheet-width 3/8) 1/2))))
      (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
        (let ((group (cl-svg:make-group scene ())))
          (apply #'draw-segment-face group :kerf 0 :cut-color mark-color :stroke-width (/ stroke-width 2) args)
          (apply #'draw-segment-face group args))))
    (with-open-file (out filename
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (cl-svg:stream-out out scene))))
