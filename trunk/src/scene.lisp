;;;;
;;;; scene.lisp
;;;;


(defpackage #:clrt-scene
  (:use :cl :clrt-camera :clrt-objects :clrt-lights :clrt-material
        :clrt-ray :linalg)
  (:export #:scene
           #:add-object
           #:add-light
           #:render))

(in-package #:clrt-scene)


(defclass scene ()
  ((camera
    :initarg :camera
    :initform (error ":camera must be specified.")
    :type camera
    :reader scene-camera)
   (objects
    :initform '()
    :reader scene-objects)
   (lights
    :initform '()
    :reader scene-lights)
   already-finalized))


(defun add-object (scene object)
  (push object (slot-value scene 'objects)))


(defun add-light (scene light)
  (push light (slot-value scene 'lights)))



(defun render (scene width height filename)
  (unless (slot-boundp scene 'already-finalized)
    (progn
      (dolist (obj (scene-objects scene))
        (finalize obj (scene-camera scene)))
      (setf (slot-value scene 'already-finalized) T)))
  (assert (consp (slot-value scene 'lights)))
  (let* ((image (make-instance 'zpng:png
                               :width width
                               :height height))
         (image-data (zpng:data-array image))
         (delta (* pi (/ (camera-fov (scene-camera scene)) 360.0)))
         (maxx (coerce (tan delta) 'single-float))
         (minx (- maxx))
         (maxy (* maxx (/ (coerce height 'single-float) (coerce width 'single-float))))
         (stepx (/ (* 2.0 maxx) (coerce width 'single-float)))
         (stepy (/ (* 2.0 maxy) (coerce height 'single-float)))
         (zero-vector (make-vector 3 :data (make-array 3
                                                       :element-type 'single-float
                                                       :initial-element 0.0))))
    (do ((y 0 (1+ y))
         (y-coord maxy (- y-coord stepy)))
        ((>= y height))
      (do* ((x 0 (1+ x))
            (x-coord minx (+ x-coord stepx))
            (image-plane-pos (make-vector 3 :data (make-array 3
                                                              :element-type 'single-float
                                                              :initial-contents (vector x-coord y-coord 1.0)))
                             (make-vector 3 :data (make-array 3
                                                              :element-type 'single-float
                                                              :initial-contents (vector x-coord y-coord 1.0)))))
           ((>= x width))
        (let ((color (trace-ray scene 
                                (make-instance 'clrt-ray:ray
                                               :origin image-plane-pos
                                               :direction (normalized
                                                           (m- image-plane-pos zero-vector))))))
          (setf (aref image-data y x 0) (min 255 (truncate (* 255 (vec-x color))))
                (aref image-data y x 1) (min 255 (truncate (* 255 (vec-y color))))
                (aref image-data y x 2) (min 255 (truncate (* 255 (vec-z color))))))))
    (zpng:write-png image filename :if-exists :supersede)))


(defun find-closest-intersection (scene ray lower-bound shadow-feeler)
  (let ((closest-match))
    (dolist (obj (scene-objects scene) closest-match)
      (let ((intersection-point (intersects obj ray
                                            :lower-bound lower-bound
                                            :shadow-feeler shadow-feeler)))
        (if closest-match
            (setf closest-match
                  (if (<= (car intersection-point) (car closest-match))
                      intersection-point
                      closest-match))
            (setf closest-match intersection-point))))))


(defun trace-ray (scene ray &optional (lower-bound 0.0) shadow-feeler)
  (let ((closest-match (find-closest-intersection scene ray lower-bound shadow-feeler))
        (black-vector (make-vector 3)))
    (if closest-match
        (destructuring-bind (dist obj ip u v normal) closest-match
            (declare (ignore dist u v))
          (let* ((mat (object-material obj))
                 (color (m* (ambient-coeff mat) (ambient-color mat))))
            (dolist (light (slot-value scene 'lights) color)
              (let* ((aux-light-dir (m- (light-pos light) ip))
                     (dist-to-light (vec-length aux-light-dir))
                     (dir-to-light (normalized aux-light-dir)))
                (unless (find-closest-intersection scene
                                                   (make-instance 'clrt-ray:ray
                                                                  :origin ip
                                                                  :direction dir-to-light)
                                                   1e-3
                                                   dist-to-light)
                  (let* ((light-color (light-color light))
                         (alpha (dot normal dir-to-light))
                         (diffuse-component (m* alpha (m. light-color (diffuse-color mat))))
                         (inv-ray-dir (m* -1.0 (ray-direction ray)))
                         (halfway-vec (normalized (m+ inv-ray-dir dir-to-light)))
                         (beta (dot normal halfway-vec))
                         (specular-component (m* beta (m. light-color (specular-color mat)))))
                    (when (< 0 alpha)
                      (setf color
                            (m+ color
                                (m* (diffuse-coeff mat) diffuse-component)
                                (m* (specular-coeff mat) specular-component))))))))))
        black-vector)))

