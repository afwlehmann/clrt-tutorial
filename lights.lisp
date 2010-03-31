;;;;
;;;; lights.lisp
;;;;


(defpackage #:clrt-lights
  (:use :cl :linalg)
  (:export #:light
           #:light-pos
           #:light-color))


(in-package #:clrt-lights)


(defclass light ()
  ((pos
    :initarg :pos
    :initform (error ":pos must be specified.")
    :type matrix
    :reader light-pos)
   (color
    :initarg :color
    :initform (make-vector 3 :data #(1.0 1.0 1.0))
    :type matrix
    :reader light-color)))
