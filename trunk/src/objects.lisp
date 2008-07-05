;;;;
;;;; objects.lisp
;;;;


(defpackage #:clrt-objects
  (:use #:cl #:linalg #:clrt-camera #:clrt-ray)
  (:export #:object
           #:object-material
           #:intersects
           #:finalize
           #:sphere))

(in-package #:clrt-objects)



(defclass object ()
  ((center
    :initarg :center
    :initform (error ":center must be specified.")
    :type matrix
    :reader object-center)
   (material
    :reader object-material)))


(defgeneric intersects (obj ray &key lower-bound shadow-feeler))


(defgeneric finalize (obj cam))

(defmethod finalize ((obj object) (cam camera))
  (setf (slot-value obj 'center)
        (world->view cam (slot-value obj 'center))))


(defun min-in-range (elements &key (lower-bound 0.0) upper-bound)
  (let ((elts (remove-if-not #'(lambda (i)
                                 (if upper-bound
                                     (<= lower-bound i upper-bound)
                                     (<= lower-bound i)))
                             elements)))
    (when elts
      (apply #'min elts))))
