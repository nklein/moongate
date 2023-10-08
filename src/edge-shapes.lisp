;;; edge-shapes.lisp

(in-package #:moongate)

(defun make-segment-edge-path (theta radius
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
  (let* ((length (* radius theta))
         (xoff (/ length 2))
         (yoff (/ support-height 2))
         (path (cl-svg:make-path)))
    (cl-svg:with-path path
      (cl-svg:move-to (- (+ xoff kerf))
                      (- (+ yoff kerf (- face-material-thickness)))))
    (make-tabbed-line path length :sx (- (+ xoff kerf)) :sy (- (+ yoff kerf))
                                  :dx 1 :dy 0
                                  :depth face-material-thickness
                                  :tabs tabs-per-face-edge
                                  :kerf kerf)
    (cl-svg:with-path path
      (cl-svg:line-to (+ xoff kerf) (+ yoff kerf (- face-material-thickness))))
    (make-tabbed-line path length :sx (+ xoff kerf) :sy (+ yoff kerf)
                                  :dx -1 :dy 0
                                  :depth face-material-thickness
                                  :tabs tabs-per-face-edge
                                  :kerf kerf)
    (cl-svg:with-path path
      (cl-svg:close-path))
    path))

(defun draw-segment-edge (scene
                          &rest
                            args
                          &key
                            (radius *outer-radius*)
                            (portion *portion*)
                            (pieces *pieces*)
                            (cut-color *cut-color*)
                            (stroke-width *stroke-width*)
                          &allow-other-keys)
  (let* ((theta (/ (* portion 2 pi)
                   pieces)))
    (cl-svg:draw scene (:path :d (apply #'make-segment-edge-path theta radius args))
                 :fill "none"
                 :stroke cut-color
                 :stroke-width stroke-width))
  scene)
