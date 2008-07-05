;;;;
;;;; camera.lisp
;;;;


(defpackage #:clrt-camera
  (:use :cl :linalg)
  (:export #:camera
           #:camera-pos
           #:camera-direction
           #:camera-fov))


(in-package #:clrt-camera)



(defclass camera ()
  ((pos
    :initarg :pos
    :initform (error ":pos must be specified.")
    :type matrix
    :reader camera-pos)
   (dir
    :type matrix
    :reader camera-direction)
   (up
    :initarg :up
    :initform (error ":up must be specified.")
    :type matrix
    :reader camera-up)
   (fov
    :initarg :fov
    :initform 110.0
    :type (real 70.0 130.0)
    :reader camera-fov)
   (w2v-matrix
    :type matrix
    :accessor w2v-matrix)))


(defmethod initialize-instance :after ((cam camera) &key look-at)
  (setf (slot-value cam 'dir)
        (normalized (m- look-at (camera-pos cam)))))


(defgeneric world->view (op1 op2))

(defmethod world->view ((cam camera) (vec matrix))
  (if (slot-boundp cam 'w2v-matrix)
      (m* (w2v-matrix cam) vec)
      (let* ((right (normalized (cross (camera-up cam) (camera-direction cam))))
             (up (normalized (cross right (camera-direction cam))))
             (dir (camera-direction cam)))
        (setf (w2v-matrix cam)
              (make-instance 'matrix
                             :rows 3
                             :cols 3
                             :data (make-array 9
                                               :element-type 'single-float
                                               :initial-contents (vector (vec-x right)
                                                                         (vec-y right)
                                                                         (vec-z right)
                                                                         (vec-x up)
                                                                         (vec-y up)
                                                                         (vec-z up)
                                                                         (vec-x dir)
                                                                         (vec-y dir)
                                                                         (vec-z dir)))))
        (m* (w2v-matrix cam) vec))))
