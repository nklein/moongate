;;; connection-constants.lisp

(in-package #:moongate)

(defparameter *tabs-per-face-edge* 15
  "Number of tabs holding edge pieces to face pieces")
(defparameter *tabs-per-support-edge* 3
  "Number of tabs the supports use to stick into the edge pieces")
(defparameter *tabs-per-support-face* 4
  "Number of tabs the supports use to stick into the face pieces")
(defparameter *supports-per-piece* 5
  "Number of supports holding each front face away from the matching back face")
