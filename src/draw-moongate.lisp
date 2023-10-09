;;; draw-moongate.lisp

(in-package #:moongate)

(defun draw-moongate* (output-stream
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
                        (tabs-per-support-edge *tabs-per-support-edge*)
                        (float-format-precision *float-format-precision*))
  (declare (ignore portion
                   pieces
                   kerf
                   cut-color
                   edge-material-thickness
                   face-material-thickness
                   support-material-thickness
                   tabs-per-face-edge
                   supports-per-piece
                   tabs-per-support-face
                   tabs-per-support-edge))
  (let* ((cl-svg:*float-format-precision* float-format-precision)
         (scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                          :width (* sheet-width dpi)
                                          :height (* sheet-height dpi)))
         (yoff (- (+ (* sheet-width 1/8) 1/2)))
         (last-height 0))
    (cl-svg:title scene "moongate")
    (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) yoff))
      (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
        (let ((group (cl-svg:make-group scene ())))
          (apply #'draw-segment-face group :kerf 0 :cut-color mark-color :stroke-width (/ stroke-width 2) args)
          (setf last-height (apply #'draw-segment-face group args))
          group)))
    (decf yoff last-height)
    (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) yoff))
      (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
        (let ((group (cl-svg:make-group scene ())))
          (apply #'draw-segment-edge group :radius outer-radius
                 :kerf 0 :cut-color mark-color :stroke-width (/ stroke-width 2) args)
          (setf last-height (apply #'draw-segment-edge group :radius outer-radius args))
          group)))
    (decf yoff (1+ last-height))
    (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) yoff))
      (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
        (let ((group (cl-svg:make-group scene ())))
          (apply #'draw-segment-edge group :radius inner-radius
                 :kerf 0 :cut-color mark-color :stroke-width (/ stroke-width 2) args)
          (setf last-height (apply #'draw-segment-edge group :radius inner-radius args))
          group)))
    (cl-svg:stream-out output-stream scene)))

(defun draw-moongate (filename
                      &rest
                        args
                      &key
                        (inner-radius *inner-radius*)
                        (outer-radius *outer-radius*)
                        (support-height *support-height*)
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
                        (tabs-per-support-edge *tabs-per-support-edge*)
                        (float-format-precision *float-format-precision*))
  (declare (ignore inner-radius
                   outer-radius
                   support-height
                   portion
                   pieces
                   kerf
                   cut-color
                   mark-color
                   stroke-width
                   sheet-width
                   sheet-height
                   dpi
                   edge-material-thickness
                   face-material-thickness
                   support-material-thickness
                   tabs-per-face-edge
                   supports-per-piece
                   tabs-per-support-face
                   tabs-per-support-edge
                   float-format-precision))
  (with-open-file (output-stream filename
                                 :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
    (apply #'draw-moongate* output-stream args)))
