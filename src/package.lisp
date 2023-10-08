;;; package.lisp

(defpackage #:moongate
  (:use #:cl)
  (:export #:*outer-radius*
           #:*inner-radius*
           #:*support-height*
           #:*portion*
           #:*pieces*)
  (:export #:*tabs-per-face-edge*
           #:*tabs-per-support-edge*
           #:*tabs-per-face-edge*
           #:*supports-per-piece*)
  (:export #:*kerf*
           #:*edge-material-thickness*
           #:*support-material-thickness*
           #:*face-material-thickness*)
  (:export #:*sheet-width*
           #:*sheet-height*
           #:*dpi*
           #:*stroke-width*
           #:*cut-color*
           #:*mark-color*
           #:*float-format-precision*)
  (:export #:draw-moongate*
           #:draw-moongate))
