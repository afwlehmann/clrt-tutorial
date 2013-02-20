;;;;
;;;; ray.lisp
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


(defpackage #:clrt-ray
  (:use #:cl #:linalg)
  (:export #:ray
           #:point-on-ray
           #:ray-origin
           #:ray-direction))

(in-package #:clrt-ray)


(defclass ray ()
  ((origin
    :initarg :origin
    :initform (error ":origin must be specified.")
    :type matrix
    :reader ray-origin)
  (direction
    :initarg :direction
    :initform (error ":direction must be specified.")
    :type matrix
    :reader ray-direction)))


(defmethod initialize-instance :after ((ray ray) &key)
  (assert (<= 0.9999
              (dot (ray-direction ray) (ray-direction ray))
              1.0001)
          nil
          ":direction must be a unit-length vector."))


(defun point-on-ray (ray dist)
  (m+ (ray-origin ray) (m* dist (ray-direction ray))))
