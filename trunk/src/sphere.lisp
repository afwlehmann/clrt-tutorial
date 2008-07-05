;;;;
;;;; sphere.lisp
;;;;


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
      (list tmin sphere (point-on-ray ray tmin)))))
