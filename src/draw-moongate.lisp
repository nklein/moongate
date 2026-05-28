;;; draw-moongate.lisp

(in-package #:moongate)

(defun draw-moongate* (output-stream
                      &rest
                        args
                      &key
                        (draw-nominal-p *draw-nominal-p*)
                        (inner-radius *inner-radius*)
                        (outer-radius *outer-radius*)
                        (support-height *support-height*)
                        (portion *portion*)
                        (pieces *pieces*)
                        (kerf *kerf*)
                        (cut-color *cut-color*)
                        (cut-opacity *cut-opacity*)
                        (mark-color *mark-color*)
                        (mark-opacity *mark-opacity*)
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
                        (overlap-edges-p *overlap-edges-p*)
                        (access-holes-p *access-holes-p*)
                        (float-format-precision *float-format-precision*))
  (declare (ignore support-height
                   portion
                   kerf
                   cut-color
                   cut-opacity
                   edge-material-thickness
                   face-material-thickness
                   support-material-thickness
                   tabs-per-face-edge
                   tabs-per-support-face
                   tabs-per-support-edge
                   access-holes-p))
  (let* ((cl-svg:*float-format-precision* float-format-precision)
         (scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                          :width (* sheet-width dpi)
                                          :height (* sheet-height dpi)))
         (yoff (- (+ (* sheet-width 1/8) 1/2)))
         (last-height 0))
    (cl-svg:title scene "moongate")

    ;; front/back
    (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) yoff))
      (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
        (let ((group (cl-svg:make-group scene ())))
          (when draw-nominal-p
            (apply #'draw-segment-face group :kerf 0
                                             :cut-color mark-color
                                             :cut-opacity mark-opacity
                                             args))
          (setf last-height (apply #'draw-segment-face group args))
          group)))

    ;; full-length edge pieces
    (when (or (not overlap-edges-p)
              (= 1 pieces)
              (<= 3 pieces)
              (< supports-per-piece 3))

      ;; full-length outside edge
      (decf yoff (1+ last-height))
      (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) yoff))
        (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
          (let ((group (cl-svg:make-group scene ())))
            (when draw-nominal-p
              (apply #'draw-segment-edge group :radius outer-radius
                                               :kerf 0
                                               :cut-color mark-color
                                               :cut-opacity mark-opacity
                                               :extra-segments 0
                                               args))
            (setf last-height (apply #'draw-segment-edge group
                                     :radius outer-radius
                                     :extra-segments 0
                                     args))
            group)))

      ;; full-length inside edge
      (decf yoff (1+ last-height))
      (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) yoff))
        (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
          (let ((group (cl-svg:make-group scene ())))
            (when draw-nominal-p
              (apply #'draw-segment-edge group :radius inner-radius
                                               :kerf 0
                                               :cut-color mark-color
                                               :cut-opacity mark-opacity
                                               :extra-segments 0
                                               args))
            (setf last-height (apply #'draw-segment-edge group :radius inner-radius args))
            group))))

    ;; short and long edge pieces
    (when (and overlap-edges-p
               (<= 2 pieces)
               (<= 3 supports-per-piece))
      ;; long outside edge
      (decf yoff (1+ last-height))
      (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) yoff))
        (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
          (let ((group (cl-svg:make-group scene ())))
            (when draw-nominal-p
              (apply #'draw-segment-edge group :radius outer-radius
                                               :kerf 0
                                               :cut-color mark-color
                                               :cut-opacity mark-opacity
                                               :extra-segments 1
                                               args))
            (setf last-height (apply #'draw-segment-edge group
                                     :radius outer-radius
                                     :extra-segments 1
                                     args))
            group)))

      ;; short outside edge
      (decf yoff (1+ last-height))
      (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) yoff))
        (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
          (let ((group (cl-svg:make-group scene ())))
            (when draw-nominal-p
              (apply #'draw-segment-edge group :radius outer-radius
                                               :kerf 0
                                               :cut-color mark-color
                                               :cut-opacity mark-opacity
                                               :extra-segments -1
                                               args))
            (setf last-height (apply #'draw-segment-edge group
                                     :radius outer-radius
                                     :extra-segments -1
                                     args))
            group)))

      ;; long inside edge
      (decf yoff (1+ last-height))
      (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) yoff))
        (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
          (let ((group (cl-svg:make-group scene ())))
            (when draw-nominal-p
              (apply #'draw-segment-edge group :radius inner-radius
                                               :kerf 0
                                               :cut-color mark-color
                                               :cut-opacity mark-opacity
                                               :extra-segments 1
                                               args))
            (setf last-height (apply #'draw-segment-edge group
                                     :radius inner-radius
                                     :extra-segments 1
                                     args))
            group)))

      ;; short inside edge
      (decf yoff (1+ last-height))
      (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) yoff))
        (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
          (let ((group (cl-svg:make-group scene ())))
            (when draw-nominal-p
              (apply #'draw-segment-edge group :radius inner-radius
                                               :kerf 0
                                               :cut-color mark-color
                                               :cut-opacity mark-opacity
                                               :extra-segments -1
                                               args))
            (setf last-height (apply #'draw-segment-edge group
                                     :radius inner-radius
                                     :extra-segments -1
                                     args))
            group))))

    ;; support pieces
    (decf yoff (1+ last-height))
    (cl-svg:transform ((cl-svg:translate (/ sheet-width 2) yoff))
      (cl-svg:transform ((cl-svg:scale dpi (- dpi)))
        (let ((group (cl-svg:make-group scene ())))
          (when draw-nominal-p
            (apply #'draw-support-piece group
                                        :kerf 0
                                        :cut-color mark-color
                                        :cut-opacity mark-opacity
                                        args))
          (setf last-height (apply #'draw-support-piece group args))
          group)))

    (cl-svg:stream-out output-stream scene)))

(defun draw-moongate (filename
                      &rest
                        args
                      &key
                        (draw-nominal-p *draw-nominal-p*)
                        (inner-radius *inner-radius*)
                        (outer-radius *outer-radius*)
                        (support-height *support-height*)
                        (portion *portion*)
                        (pieces *pieces*)
                        (kerf *kerf*)
                        (cut-color *cut-color*)
                        (cut-opacity *cut-opacity*)
                        (mark-color *mark-color*)
                        (mark-opacity *mark-opacity*)
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
                        (overlap-edges-p *overlap-edges-p*)
                        (access-holes-p *access-holes-p*)
                        (float-format-precision *float-format-precision*))
  (declare (ignore draw-nominal-p
                   inner-radius
                   outer-radius
                   support-height
                   portion
                   pieces
                   kerf
                   cut-color
                   cut-opacity
                   mark-color
                   mark-opacity
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
                   overlap-edges-p
                   access-holes-p
                   float-format-precision))
  (with-open-file (output-stream filename
                                 :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
    (apply #'draw-moongate* output-stream args)))


#+(or)
(moongate:draw-moongate #P"/tmp/mg.svg"
                        :outer-radius 6
                        :inner-radius 4
                        :portion 3/4
                        :pieces 3
                        :support-height 1
                        :sheet-width 30
                        :sheet-height 15
                        :kerf 1/100
                        :draw-nominal-p nil
                        :face-material-thickness 1/8
                        :edge-material-thickness 1/8
                        :support-material-thickness 1/8
                        :tabs-per-support-face 3
                        :tabs-per-support-edge 1
                        :access-holes-p nil
                        :supports-per-piece 3
                        :tabs-per-face-edge 12)

#+(or)
(moongate:draw-moongate #P"/tmp/mg.svg"
                        :draw-nominal-p nil
                        :access-holes-p t)
