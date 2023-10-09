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

(defun make-support-tab-holes-in-edge-path (xx
                                            &key
                                              (kerf *kerf*)
                                              (support-height *support-height*)
                                              (support-material-thickness *support-material-thickness*)
                                              (face-material-thickness *face-material-thickness*)
                                              (tabs-per-support-edge *tabs-per-support-edge*)
                                            &allow-other-keys)
  (let* ((path (cl-svg:make-path))
         (length (- support-height (* 2 face-material-thickness)))
         (tab-distance-on-center (/ length tabs-per-support-edge))
         (kerf/2 (/ kerf 2))
         (slot-width/2 (+ (/ support-material-thickness 2) kerf/2))
         (slot-height/2 (+ (/ tab-distance-on-center 4) kerf/2)))
    (loop :repeat tabs-per-support-edge
          :for yy :from (- (+ (/ tab-distance-on-center 2) face-material-thickness)
                           (/ support-height 2))
                  :by tab-distance-on-center
          :do (cl-svg:with-path path
                (cl-svg:move-to (- xx slot-width/2)
                                (- yy slot-height/2))
                (cl-svg:line-to (+ xx slot-width/2)
                                (- yy slot-height/2))
                (cl-svg:line-to (+ xx slot-width/2)
                                (+ yy slot-height/2))
                (cl-svg:line-to (- xx slot-width/2)
                                (+ yy slot-height/2))
                (cl-svg:close-path)))
    path)
  )

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
                            (cut-color *cut-color*)
                            (stroke-width *stroke-width*)
                          &allow-other-keys)
  (let* ((theta (/ (* portion 2 pi)
                   pieces))
         (length (* theta radius))
         (support-distance-on-center (/ length supports-per-piece)))
    (cl-svg:draw scene (:path :d (apply #'make-segment-edge-path length args))
                 :fill "none"
                 :stroke cut-color
                 :stroke-width stroke-width)

    (loop :repeat supports-per-piece
          :for xx :from (- (/ support-distance-on-center 2)
                           (/ length 2))
                  :by support-distance-on-center
          :do (cl-svg:draw scene (:path :d (apply #'make-support-tab-holes-in-edge-path xx args))
                           :fill "none"
                           :stroke cut-color
                           :stroke-width stroke-width)))
  (+ support-height (* 2 kerf)))
