;;; edge-shapes.lisp

(in-package #:moongate)

(defun make-segment-edge-path (length
                               &key
                                 (kerf *kerf*)
                                 (support-height *support-height*)
                                 (tabs-per-face-edge *tabs-per-face-edge*)
                                 #+(or)
                                 (tabs-per-support-edge *tabs-per-support-edge*)
                                 (face-material-thickness *face-material-thickness*)
                                 #+(or)
                                 (support-material-thickness *support-material-thickness*)
                               &allow-other-keys)
  (let* ((xoff (/ length 2))
         (yoff (/ support-height 2))
         (kerf/2 (/ kerf 2))
         (path (cl-svg:make-path)))
    (cl-svg:with-path path
      (cl-svg:move-to (- (+ xoff kerf/2))
                      (- (+ yoff kerf/2 (- face-material-thickness)))))
    (make-tabbed-line path length :sx (- (+ xoff kerf/2)) :sy (- (+ yoff kerf/2))
                                  :dx 1 :dy 0
                                  :depth face-material-thickness
                                  :tabs tabs-per-face-edge
                                  :kerf kerf)
    (cl-svg:with-path path
      (cl-svg:line-to (+ xoff kerf/2) (+ yoff kerf/2 (- face-material-thickness))))
    (make-tabbed-line path length :sx (+ xoff kerf/2) :sy (+ yoff kerf/2)
                                  :dx -1 :dy 0
                                  :depth face-material-thickness
                                  :tabs tabs-per-face-edge
                                  :kerf kerf)
    (cl-svg:with-path path
      (cl-svg:close-path))
    path))

(defun support-length (support-height material-thickness)
  (- support-height (* 2 material-thickness)))

(defun make-support-tab-holes-in-edge-path (xx
                                            &key
                                              (kerf *kerf*)
                                              (support-height *support-height*)
                                              (support-material-thickness *support-material-thickness*)
                                              (face-material-thickness *face-material-thickness*)
                                              (tabs-per-support-edge *tabs-per-support-edge*)
                                            &allow-other-keys)
  (let* ((path (cl-svg:make-path))
         (length (support-length support-height face-material-thickness))
         (tab-distance-on-center (/ length tabs-per-support-edge))
         (kerf/2 (/ kerf 2))
         (slot-width/2 (- (/ support-material-thickness 2) kerf/2))
         (slot-height/2 (- (/ tab-distance-on-center 4) kerf/2)))
    (loop :repeat tabs-per-support-edge
          :for yy :from (- (+ (/ tab-distance-on-center 2) face-material-thickness)
                           (/ support-height 2))
                  :by tab-distance-on-center
          :do (cl-svg:with-path path
                (cl-svg:move-to (- xx slot-width/2)
                                (- yy slot-height/2))
                (cl-svg:line-to (- xx slot-width/2)
                                (+ yy slot-height/2))
                (cl-svg:line-to (+ xx slot-width/2)
                                (+ yy slot-height/2))
                (cl-svg:line-to (+ xx slot-width/2)
                                (- yy slot-height/2))
                (cl-svg:close-path)))
    path))

(defun make-access-holes-in-edge-path (xx width
                                            &key
                                              (kerf *kerf*)
                                              (support-height *support-height*)
                                              (support-material-thickness *support-material-thickness*)
                                              (face-material-thickness *face-material-thickness*)
                                            &allow-other-keys)
  (let* ((path (cl-svg:make-path))
         (height (* (- support-height (* 2 face-material-thickness)) 3/4))
         (kerf/2 (/ kerf 2))
         (slot-width/2 (- (/ width 2) (/ support-material-thickness 2) kerf/2))
         (slot-height/2 (- (/ height 2) kerf/2))
         (yy 0))
    (cl-svg:with-path path
      (cl-svg:move-to (- xx slot-width/2)
                      (- yy slot-height/2))
      (cl-svg:line-to (- xx slot-width/2)
                      (+ yy slot-height/2))
      (cl-svg:line-to (+ xx slot-width/2)
                      (+ yy slot-height/2))
      (cl-svg:line-to (+ xx slot-width/2)
                      (- yy slot-height/2))
      (cl-svg:close-path))
    path))

(defun draw-segment-edge (scene
                          &rest
                            args
                          &key
                            (radius *outer-radius*)
                            (portion *portion*)
                            (pieces *pieces*)
                            (kerf *kerf*)
                            (supports-per-piece *supports-per-piece*)
                            (support-height *support-height*)
                            (tabs-per-face-edge *tabs-per-face-edge*)
                            (cut-color *cut-color*)
                            (cut-opacity *cut-opacity*)
                            (access-holes-p *access-holes-p*)
                            (extra-segments 0)
                          &allow-other-keys)
  (let* ((scale (/ (+ supports-per-piece extra-segments) supports-per-piece))
         (portion (* portion scale))
         (supports-per-piece (* supports-per-piece scale))
         (tabs-per-face-edge (* tabs-per-face-edge scale))
         (theta (/ (* portion 2 pi)
                   pieces))
         (length (* theta radius))
         (support-distance-on-center (/ length supports-per-piece))
         (support-distance-on-center/2 (/ support-distance-on-center 2)))
    (cl-svg:draw scene (:path :d (apply #'concatenate 'string
                                        (apply #'make-segment-edge-path length
                                               :tabs-per-face-edge tabs-per-face-edge
                                               args)
                                        (loop :for k :from 0 :below  supports-per-piece
                                              :for xx :from (- support-distance-on-center/2
                                                               (/ length 2))
                                              :by support-distance-on-center
                                              :collect (apply #'make-support-tab-holes-in-edge-path xx args)
                                              :when (and access-holes-p (not (zerop k)))
                                                :collect (apply #'make-access-holes-in-edge-path
                                                                (- xx support-distance-on-center/2)
                                                                (* support-distance-on-center 3/4)
                                                                args))))
                 :fill cut-color
                 :fill-opacity cut-opacity))
  (+ support-height (* 2 kerf)))

(defun draw-support-piece (scene
                           &key
                             (kerf *kerf*)
                             (inner-radius *inner-radius*)
                             (outer-radius *outer-radius*)
                             (face-material-thickness *face-material-thickness*)
                             (edge-material-thickness *edge-material-thickness*)
                             (support-height *support-height*)
                             (cut-color *cut-color*)
                             (cut-opacity *cut-opacity*)
                             (tabs-per-support-edge *tabs-per-support-edge*)
                             (tabs-per-support-face *tabs-per-support-face*)
                             (access-holes-p *access-holes-p*)
                           &allow-other-keys)
  (let* ((height (support-length support-height face-material-thickness))
         (width (support-length (- outer-radius inner-radius)
                                edge-material-thickness))
         (height/2 (/ height 2))
         (width/2 (/ width 2))
         (kerf/2 (/ kerf 2)))

    (cl-svg:draw scene (:path :d (apply #'concatenate 'string
                                        (cl-svg:move-to (- (+ width/2 kerf/2))
                                                        (- (+ height/2 kerf/2 face-material-thickness)))
                                        (let ((path (cl-svg:make-path)))
                                          (make-tabbed-line path width
                                                            :sx (- (+ width/2 kerf/2))
                                                            :sy (- (+ height/2 kerf/2 face-material-thickness))
                                                            :dx 1
                                                            :dy 0
                                                            :depth face-material-thickness
                                                            :tabs tabs-per-support-face
                                                            :kerf kerf)
                                          path)
                                        (cl-svg:line-to (+ width/2 kerf/2)
                                                        (- (+ height/2 kerf/2 face-material-thickness)))

                                        (let ((path (cl-svg:make-path)))
                                          (make-tabbed-line path height
                                                            :sx (+ width/2 kerf/2)
                                                            :sy (- (+ height/2 kerf/2))
                                                            :dx 0
                                                            :dy 1
                                                            :depth edge-material-thickness
                                                            :tabs tabs-per-support-edge
                                                            :kerf kerf)
                                          path)
                                        (cl-svg:line-to (+ width/2 kerf/2)
                                                        (+ height/2 kerf/2 edge-material-thickness))
                                        (let ((path (cl-svg:make-path)))
                                          (make-tabbed-line path width
                                                            :sx (+ width/2 kerf/2)
                                                            :sy (+ height/2 kerf/2 face-material-thickness)
                                                            :dx -1
                                                            :dy 0
                                                            :depth face-material-thickness
                                                            :tabs tabs-per-support-face
                                                            :kerf kerf)
                                          path)
                                        (cl-svg:line-to (- (+ width/2 kerf/2))
                                                        (+ height/2 kerf/2 edge-material-thickness))

                                        (let ((path (cl-svg:make-path)))
                                          (make-tabbed-line path height
                                                            :sx (- (+ width/2 kerf/2))
                                                            :sy (+ height/2 kerf/2)
                                                            :dx 0
                                                            :dy -1
                                                            :depth edge-material-thickness
                                                            :tabs tabs-per-support-edge
                                                            :kerf kerf)
                                          path)
                                        (cl-svg:close-path)
                                        (when access-holes-p
                                          (list (make-access-holes-in-edge-path 0 (* width 3/4)
                                                                                :support-height height
                                                                                :support-material-thickness edge-material-thickness)))))
                 :fill cut-color
                 :fill-opacity cut-opacity)
    height))
