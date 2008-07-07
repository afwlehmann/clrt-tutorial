;;;;
;;;; material.lisp
;;;;


(defpackage #:clrt-material
  (:use :cl :linalg)
  (:export #:material
           #:ambient-color
           #:diffuse-color
           #:specular-color
           #:ambient-coeff
           #:diffuse-coeff
           #:specular-coeff
           #:roughness))


(in-package #:clrt-material)


(defclass material ()
  ((ambient-color
    :initarg :ambient-color
    :initform (make-vector 3)
    :type matrix
    :reader ambient-color)
   (diffuse-color
    :initarg :diffuse-color
    :initform (make-vector 3 :data #(1.0 1.0 1.0))
    :type matrix
    :reader diffuse-color)
   (specular-color
    :initarg :specular-color
    :initform (make-vector 3)
    :type matrix
    :reader specular-color)
   (ambient-coeff
    :initarg :ambient-coeff
    :initform 0.0
    :type (real 0.0 1.0)
    :reader ambient-coeff)
   (diffuse-coeff
    :initarg :diffuse-coeff
    :initform 1.0
    :type (real 0.0 1.0)
    :reader diffuse-coeff)
   (specular-coeff
    :initarg :specular-coeff
    :initform 0.0
    :type (real 0.0 1.0)
    :reader specular-coeff)
   (roughness
    :initarg :specular-coeff
    :initform 0
    :type (integer 0)
    :reader roughness)))


(defmethod initialize-instance :after ((mat material) &key)
  (assert (= (+ (ambient-coeff mat) (diffuse-coeff mat) (specular-coeff mat))
             1.0)
          nil
          ":ambient-coeff, :diffuse-coeff and :specular-coeff must add up to 1.0."))
