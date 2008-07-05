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
   (:file "objects" :depends-on ("linalg" "camera"))
   (:file "scene" :depends-on ("linalg" "camera" "objects"))))
