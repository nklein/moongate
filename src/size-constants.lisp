;;; size-constants.lisp

(in-package #:moongate)

(defparameter *outer-radius* 96
  "Outer radius of gate (in inches)")
(defparameter *inner-radius* 86
  "Inner radius of gate (in inches)")
(defparameter *support-height* 10
  "Seperation between front of ring and back of ring (in inches)")

(defparameter *portion* 3/4
  "Portion of the circle we are constructing")
(defparameter *pieces* 11
  "Number of pieces we are breaking that portion of the circle into")
