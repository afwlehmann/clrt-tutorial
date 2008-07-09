;;;;
;;;; clrt.asd
;;;;


(require 'asdf)


(asdf:defsystem #:clrt
  :description "clrt: a simple common lisp raytracer"
  :depends-on (#:zpng)
  :components
  ((:file "linalg")
   (:file "camera" :depends-on ("linalg"))
   (:file "objects" :depends-on ("linalg" "camera" "ray" "material"))
   (:file "scene" :depends-on ("linalg" "camera" "objects" "lights" "material"))
   (:file "sphere" :depends-on ("linalg" "objects"))
   (:file "ray" :depends-on ("linalg"))
   (:file "cube" :depends-on ("linalg" "objects"))
   (:file "lights" :depends-on ("linalg"))
   (:file "material" :depends-on ("linalg"))))


