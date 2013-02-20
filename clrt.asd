;;;;
;;;; clrt.asd
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


(require 'asdf)


(asdf:defsystem #:clrt
  :description "clrt: a simple common lisp raytracer"
  :author "Alexander Lehmann <afwlehmann@googlemail.com>"
  :license "Apache License 2.0"
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


