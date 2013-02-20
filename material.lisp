;;;;
;;;; material.lisp
;;;;
;;;; Copyright 2009 Alexander Lehmann
;;;;
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;;
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.


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
    :initform (make-vector 3 :data (make-array 3
                                               :element-type 'single-float
                                               :initial-element 1.0))
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
    :initarg :roughness
    :initform 50
    :type (integer 0)
    :reader roughness)))


(defmethod initialize-instance :after ((mat material) &key)
  (assert (= (+ (ambient-coeff mat) (diffuse-coeff mat) (specular-coeff mat))
             1.0)
          nil
          ":ambient-coeff, :diffuse-coeff and :specular-coeff must sum up to 1.0."))
