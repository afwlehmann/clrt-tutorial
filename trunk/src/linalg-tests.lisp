;;;;
;;;; linalg-tests.lsp
;;;;


(defpackage :linalg-tests
  (:use :cl :linalg :lisp-unit))


;;; Force the export of the symbols matrix-(rows|cols|data) which are neccessary
;;; for the execution of the tests.
(in-package :linalg)

(export '(matrix-rows matrix-cols matrix-data))


(in-package :linalg-tests)


(defun matrix= (a b)
  (and (= (matrix-rows a) (matrix-rows b))
       (= (matrix-cols a) (matrix-cols b))
       (equalp (matrix-data a) (matrix-data b))))


;;; Some constant matrices that will be used throughout the tests
(defparameter *A*
  (make-instance 'matrix
                 :rows 2
                 :cols 4
                 :data #(1.0 2.0 3.0 4.0 
                         5.0 6.0 7.0 8.0)))

(defparameter *B*
  (make-instance 'matrix
                 :rows 4
                 :cols 4
                 :data #(-2.0 -4.0 -0.5 9.0
                         -1.0 -3.0  2.0 4.0
                          5.0  7.0  0.0 1.0
                          2.0  3.0  4.0 5.0)))

(defparameter *rv*
  (make-vector 4 :data #(42.0 23.0 -23.0 -42.0) :orientation :row))

(defparameter *cv*
  (make-vector 4 :data #(7.0 8.0 -71.0 0.1)))


;;; Conversion of an array into a column vector
(define-test column-vector-from-array
    (assert-true
     (let ((cv (make-vector 4 :data #(1 2 3 4))))
       (and (= (matrix-at cv 0 0) 1)
            (= (matrix-at cv 1 0) 2)
            (= (matrix-at cv 2 0) 3)
            (= (matrix-at cv 3 0) 4)))))


;;; Conversion of an array into a row vector
(define-test row-vector-from-array
    (assert-true
     (let ((rv (make-vector 4 :data #(1 2 3 4) :orientation :row)))
       (and (= (matrix-at rv 0 0) 1)
            (= (matrix-at rv 0 1) 2)
            (= (matrix-at rv 0 2) 3)
            (= (matrix-at rv 0 3) 4)))))


;;; Matrix addition
(define-test matrix+
    (assert-equality #'matrix=
                     (m+ *A* *A* *A*)
                     (make-instance 'matrix
                                    :rows 2
                                    :cols 4
                                    :data #( 3.0  6.0  9.0 12.0
                                            15.0 18.0 21.0 24.0))))


;;; Matrix subtraction
(define-test matrix-
    (assert-equality #'matrix=
                     (m- *A* *A* *A*)
                     (make-instance 'matrix
                                    :rows 2
                                    :cols 4
                                    :data #(-1.0 -2.0 -3.0 -4.0
                                            -5.0 -6.0 -7.0 -8.0))))


;;; Matrix multiplication
(define-test m*
    (assert-equality #'matrix=
                     (m* *A* *B*)
                     (make-instance 'matrix
                                    :rows 2
                                    :cols 4
                                    :data #(19.0 23.0 19.5  40.0
                                            35.0 35.0 41.5 116.0))))


;;; Multiplication of a matrix and a scalar value
(define-test matrix-scalar-multiplication
    (assert-equality #'matrix=
                     (m* *A* 0.5)
                     (make-instance 'matrix
                                    :rows 2
                                    :cols 4
                                    :data #(0.5 1.0 1.5 2.0
                                            2.5 3.0 3.5 4.0))))


;;; Multiplication of a scalar value and a matrix
(define-test scalar-matrix-multiplication
    (assert-equality #'matrix=
                     (m* 0.5 *A*)
                     (make-instance 'matrix
                                    :rows 2
                                    :cols 4
                                    :data #(0.5 1.0 1.5 2.0
                                            2.5 3.0 3.5 4.0))))


;;; Transpose of a matrix
(define-test matrix-transpose
    (assert-equality #'matrix=
                     (transposed *A*)
                     (make-instance 'matrix
                                    :rows 4
                                    :cols 2
                                    :data #(1.0 5.0 2.0 6.0
                                            3.0 7.0 4.0 8.0))))


;;; Multiplication of a matrix and a column vector
(define-test matrix-column-vector-multiplication
    (assert-equality #'matrix=
                     (m* *B* *cv*)
                     (make-vector 4 :data #(-9.6 -172.6 91.1 -245.5))))


;;; Multiplication of a row vector and a matrix
(define-test row-vector-matrix-multiplication
    (assert-equality #'matrix=
                     (m* *rv* *B*)
                     (make-vector 4 :data #(-306.0 -524.0 -143.0 237.0) :orientation :row)))


;;; Multiplication of row and column vector
(define-test row-column-vector-multiplication
    (assert-equal 2106.8 (m* *rv* *cv*)))


;;; Length of a vector
(define-test length-of-vector
    (assert-equal 71.791434 (vec-length *cv*)))


;;; Multiplication with arbitrary addends
(define-test mult
    (assert-equal 406781.4 (mult *rv* *B* (transposed *B*) *cv*)))


;;; Copying a matrix
(define-test copy-matrix
    (assert-equality #'matrix= *A* (copy-matrix *A*)))


;;; Normalization
(define-test normalized-vector
    (assert-equal 1.0
                  (fround
                   (vec-length
                    (normalized
                     (make-vector 10
                                  :generator #'(lambda (k) (random 100.0))))))))


;;; Dot-product
(define-test dot-product
    (assert-equal 14.0
                  (dot (make-vector 3 :data #(1.0 2.0 3.0))
                       (make-vector 3 :data #(1.0 2.0 3.0)))))
