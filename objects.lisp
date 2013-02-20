;;;;
;;;; objects.lisp
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


(defpackage #:clrt-objects
  (:use #:cl #:linalg #:clrt-camera #:clrt-ray #:clrt-material)
  (:export #:object
           #:object-material
           #:intersects
           #:finalize
           #:sphere
           #:cube))

(in-package #:clrt-objects)



(defclass object ()
  ((center
    :initarg :center
    :initform (error ":center must be specified.")
    :type matrix
    :reader object-center)
   (material
    :initarg :material
    :initform (error ":material must be specified.")
    :type material
    :reader object-material)))


(defgeneric intersects (obj ray &key lower-bound shadow-feeler))


(defgeneric finalize (obj cam))

(defmethod finalize ((obj object) (cam camera))
  (setf (slot-value obj 'center)
        (world->view cam (slot-value obj 'center))))


(defun min-in-range (elements &key (lower-bound 0.0) upper-bound (key #'identity))
  (let ((elts (remove-if-not #'(lambda (i)
                                 (if upper-bound
                                     (<= lower-bound i upper-bound)
                                     (<= lower-bound i)))
                             elements :key key)))
    (when elts
      (reduce #'(lambda (a b) (if (<= (funcall key a) (funcall key b))
                                  a
                                  b))
              elts))))


(defun intersects-face (origin up right ray test-fn)
  (let* ((a (vec-x right))
         (b (vec-x up))
         (c (- (vec-x (ray-direction ray))))
         (d (vec-y right))
         (e (vec-y up))
         (f (- (vec-y (ray-direction ray))))
         (g (vec-z right))
         (h (vec-z up))
         (i (- (vec-z (ray-direction ray))))
         (det (- (+ (* a e i) (* b f g) (* c d h))
                 (+ (* c e g) (* b d i) (* a f h)))))
    (when (/= det 0)
      (let* ((rhs (m- (ray-origin ray) origin))
             (u (/ (+ (* (- (* e i) (* f h)) (vec-x rhs))
                      (* (- (* c h) (* b i)) (vec-y rhs))
                      (* (- (* b f) (* c e)) (vec-z rhs)))
                   det))
             (v (/ (+ (* (- (* f g) (* d i)) (vec-x rhs))
                      (* (- (* a i) (* c g)) (vec-y rhs))
                      (* (- (* c d) (* a f)) (vec-z rhs)))
                   det))
             (dist (/ (+ (* (- (* d h) (* e g)) (vec-x rhs))
                         (* (- (* b g) (* a h)) (vec-y rhs))
                         (* (- (* a e) (* b d)) (vec-z rhs)))
                      det)))
        (when (funcall test-fn u v)
          (values dist u v))))))


