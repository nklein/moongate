;;; svg-constants.lisp

(in-package #:moongate)

(defparameter *sheet-width* 48)
(defparameter *sheet-height* 36)

(defparameter *dpi* 96
  "Dots per inch (inkscape assumes 96)")

(defparameter *stroke-width* 1/1000
  "Width of strokes in drawing (in inches)")
(defparameter *cut-color* "red"
  "Color to mark the lines to be cut")
(defparameter *mark-color* "black"
  "Color to mark the lines to be engraved")

(defparameter *float-format-precision* 8
  "Number of decimal places when printing floating point numbers in SVG files")
