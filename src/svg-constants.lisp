;;; svg-constants.lisp

(in-package #:moongate)

(defparameter *sheet-width* 48)
(defparameter *sheet-height* 36)

(defparameter *dpi* 96
  "Dots per inch (inkscape assumes 96)")

(defparameter *cut-color* "red"
  "Color to fill shapes to be cut")
(defparameter *cut-opacity* 1/4
  "Opacity of cut pieces")
(defparameter *mark-color* "black"
  "Color to mark the shapes to be engraved")
(defparameter *mark-opacity* 1/4
  "Opacity of marking pieces")

(defparameter *draw-nominal-p* nil
  "Whether to draw the kerf-free components")

(defparameter *float-format-precision* 8
  "Number of decimal places when printing floating point numbers in SVG files")
