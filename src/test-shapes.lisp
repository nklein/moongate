;;; draw-moongate.lisp

(in-package #:moongate)

(defun draw-depth-test-shape (group
                              obj-width
                              obj-height
                              &key
                                (stops 5)
                                (delta 1/64)
                                (material-thickness *face-material-thickness*)
                                (cut-color *cut-color*)
                                (cut-opacity *cut-opacity*)
                                (kerf *kerf*)
                              &allow-other-keys)
  (cl-svg:draw group (:path :d (concatenate 'string
                                            (cl-svg:move-to (- (/ obj-width 2))
                                                            (- (/ obj-height 2)))
                                            (cl-svg:line-to (/ obj-width 2)
                                                            (- (/ obj-height 2)))

                                            (let ((path (cl-svg:make-path)))
                                              (loop :with sx := (+ (/ obj-width 2))
                                                    :for sy := (- (/ obj-height 2)) :then (+ sy
                                                                                             (* thickness 2))
                                                    :for thickness := (+ material-thickness
                                                                         (* (1- stops) delta 1/2))
                                                      :then (- thickness delta)
                                                    :repeat stops
                                                    :do (make-tabbed-line path (* thickness 2)
                                                                          :sx sx
                                                                          :sy sy
                                                                          :dx 0
                                                                          :dy 1
                                                                          :tabs 1
                                                                          :kerf kerf
                                                                          :depth (* material-thickness -2)))
                                              path)
                                            (cl-svg:line-to-r (- (* material-thickness 3))
                                                              0)
                                            (cl-svg:close-path)))
               :fill cut-color
               :fill-opacity cut-opacity))

(defun draw-depth-test* (output-stream
                         &rest
                           args
                         &key
                           (stops 5)
                           (delta 1/64)
                           (draw-nominal nil)
                           (kerf *kerf*)
                           (cut-color *cut-color*)
                           (cut-opacity *cut-opacity*)
                           (mark-color *mark-color*)
                           (mark-opacity *mark-opacity*)
                           (dpi *dpi*)
                           (material-thickness *face-material-thickness*)
                           (float-format-precision *float-format-precision*))
  (declare (ignore delta
                   kerf
                   cut-color
                   cut-opacity))
  (let* ((cl-svg:*float-format-precision* float-format-precision)
         (obj-height (* material-thickness (+ (* 2 stops) 1)))
         (obj-width (* material-thickness 5))
         (sheet-width (+ obj-width (* 2 material-thickness)))
         (sheet-height (+ obj-height (* 2 material-thickness)))
         (scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                          :width (* sheet-width dpi)
                                          :height (* sheet-height dpi))))
    (cl-svg:title scene "depth-test")
    (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) (/ sheet-height -2)))
      (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
        (let ((group (cl-svg:make-group scene ())))
          (when draw-nominal
            (apply #'draw-depth-test-shape group obj-width obj-height
                   :kerf 0
                   :cut-color mark-color
                   :cut-opacity mark-opacity
                   args))
          (apply #'draw-depth-test-shape group obj-width obj-height args)
          group)))
    (cl-svg:stream-out output-stream scene)))

(defun draw-depth-test (filename
                        &rest
                          args
                        &key
                          (stops 5)
                          (delta 1/64)
                          (draw-nominal nil)
                          (kerf *kerf*)
                          (cut-color *cut-color*)
                          (cut-opacity *cut-opacity*)
                          (mark-color *mark-color*)
                          (mark-opacity *mark-opacity*)
                          (dpi *dpi*)
                          (material-thickness *face-material-thickness*)
                          (float-format-precision *float-format-precision*))
  (declare (ignore stops
                   delta
                   draw-nominal
                   kerf
                   cut-color
                   cut-opacity
                   mark-color
                   mark-opacity
                   dpi
                   material-thickness
                   float-format-precision))
  (with-open-file (output-stream filename
                                 :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
    (apply #'draw-depth-test* output-stream args)))
