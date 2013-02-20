;;;;
;;;; cube.lisp
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


(in-package #:clrt-objects)


(defclass cube (object)
  (fll
   flr
   fur
   ful
   bll
   blr
   bur
   bul
   ;;; The following slots will resemble a list consisting of
   ;;; the face's origin, it's up- and right-vector as well as
   ;;; the face's surface normal.
   front
   back
   left
   right
   top
   bottom))


(defmethod initialize-instance :after ((cube cube) &key width height depth)
  (assert (< 0 width)
          nil
          ":width must be > 0.")
  (assert (< 0 height)
          nil
          ":height must be > 0.")
  (assert (< 0 depth)
          nil
          ":depth must be > 0.")
  (let ((cx (vec-x (object-center cube)))
        (cy (vec-y (object-center cube)))
        (cz (vec-z (object-center cube)))
        (w/2 (/ width 2.0))
        (h/2 (/ height 2.0))
        (d/2 (/ depth 2.0)))
    (macrolet ((prep-edge (edge-name opx opy opz)
                 `(setf (slot-value cube ,edge-name)
                        (make-vector 3
                                     :data (make-array 3
                                                       :element-type 'single-float
                                                       :initial-contents (vector (,opx cx w/2)
                                                                                 (,opy cy h/2)
                                                                                 (,opz cz d/2)))))))
      (prep-edge 'fll - - -)
      (prep-edge 'flr + - -)
      (prep-edge 'fur + + -)
      (prep-edge 'ful - + -)
      (prep-edge 'bll - - +)
      (prep-edge 'blr + - +)
      (prep-edge 'bur + + +)
      (prep-edge 'bul - + +))))


(defmethod finalize ((cube cube) (cam camera))
  (dolist (edge '(fll flr fur ful bll blr bur bul))
    (setf (slot-value cube edge)
          (world->view cam (slot-value cube edge))))
  (macrolet ((prep-face (face-name lower-left upper-left lower-right)
               (let ((up (gensym))
                     (right (gensym))
                     (normal (gensym)))
                 `(let* ((,up (m- (slot-value cube ,upper-left) (slot-value cube ,lower-left)))
                         (,right (m- (slot-value cube ,lower-right) (slot-value cube ,lower-left))) 
                         (,normal (normalized (cross ,up ,right))))
                    (setf (slot-value cube ,face-name)
                          (list (slot-value cube ,lower-left) ,up ,right ,normal))))))
    (prep-face 'front 'fll 'ful 'flr)
    (prep-face 'back 'blr 'bur 'bll)
    (prep-face 'left 'bll 'bul 'fll)
    (prep-face 'right 'flr 'fur 'blr)
    (prep-face 'top 'ful 'bul 'fur)
    (prep-face 'bottom 'blr 'flr 'bll))
  T)


(defmethod intersects ((cube cube) (ray ray) &key (lower-bound 0.0) shadow-feeler)
  (let ((intersection-points
         (loop for side in '(front back left right top bottom)
              for ip = (destructuring-bind (origin up right normal)
                           (slot-value cube side)
                         (multiple-value-bind (dist u v)
                             (intersects-face origin up right ray #'(lambda (u v) (and (<= 0 u 1)
                                                                                       (<= 0 v 1))))
                           (when dist
                             (list dist cube (point-on-ray ray dist) u v normal))))
              unless (null ip) collect ip)))
    (min-in-range intersection-points
                  :lower-bound lower-bound
                  :upper-bound shadow-feeler
                  :key #'car)))
