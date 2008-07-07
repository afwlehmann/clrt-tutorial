;;;;
;;;; light.lisp
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
    :reader light-pos
    :type matrix)
   (color
    :initarg :color
    :initform (make-vector 3 :data (make-array 3
                                               :element-type 'single-float
                                               :initial-element 1.0))
    :reader light-color
    :type matrix)))

