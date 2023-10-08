;;; material-constants.lisp

(in-package #:moongate)

(defparameter *kerf* 1/10
  "Width of cutting tool (in inches)")

(defparameter *edge-material-thickness* 1/8
  "Thickness of material bent around the edges (in inches)")
(defparameter *support-material-thickness* 1/8
  "Thickness of material used for support pieces (in inches)")
(defparameter *face-material-thickness* 1/8
  "Thickness of material used for the face pieces (in inches)")
