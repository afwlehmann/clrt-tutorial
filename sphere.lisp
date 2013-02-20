;;;;
;;;; sphere.lisp
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


(defclass sphere (object)
  ((radius
    :initarg :radius
    :initform (error ":radius must be specified.")
    :type single-float
    :reader sphere-radius)))


(defmethod intersects ((sphere sphere) (ray ray) &key (lower-bound 0.0) shadow-feeler)
  (let* ((ro (ray-origin ray))
         (rd (ray-direction ray))
         (c (object-center sphere))
         (r (sphere-radius sphere))
         (ro*rd (dot ro rd))
         (rd*rd (dot rd rd))
         (c*rd (dot c rd))
         (discr (- (* 4 (expt (- ro*rd c*rd) 2))
                   (* 4 rd*rd (- (dot ro ro) (* 2 (dot ro c)) (- (dot c c)) (* r r)))))
         (tmin (min-in-range (cond
                               ((< discr 0) nil)
                               ((= discr 0) (list (/ (* -2 (- ro*rd c*rd)) (* 2 rd*rd))))
                               (t (let ((root (sqrt discr)))
                                    (list (/ (+ (* -2 (- ro*rd c*rd)) root) (* 2 rd*rd))
                                          (/ (- (* -2 (- ro*rd c*rd)) root) (* 2 rd*rd))))))
                             :lower-bound lower-bound
                             :upper-bound shadow-feeler)))
    (when tmin
      (let ((ip (point-on-ray ray tmin)))
        (list tmin sphere ip 
            nil nil ;; TODO
            (normalized (m- ip c)))))))
