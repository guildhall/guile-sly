;;; Sly
;;; Copyright (C) 2014 David Thompson <davet@gnu.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Vector math.
;;
;;; Code:

(define-module (sly vector)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:export (vector2? vector3? vector4?
                     vector-length= v=
                     vx vy vz vw
                     polar-vector
                     v+ v- v* vdot vcross
                     magnitude normalize))

(define (vector-dimensionality? v d)
  (and (vector? v) (= (vector-length v) d)))

(define (vector2? v)
  "Return #t if V is a 2D vector, #f otherwise."
  (vector-dimensionality? v 2))

(define (vector3? v)
  "Return #t if V is a 3D vector, #f otherwise."
  (vector-dimensionality? v 3))

(define (vector4? v)
  "Return #t if V is a 4D vector, #f otherwise."
  (vector-dimensionality? v 4))

(define (vector-length= v1 v2)
  "Return #t if V1 and V2 are of the same dimensionality, #f
otherwise."
  (= (vector-length v1)
     (vector-length v2)))

(define (v= . vectors)
  "Return #t if all arguments are equivalent vectors, #f otherwise."
  (apply vector= = vectors))

(define (vx v)
  "Return the first component of the vector V."
  (vector-ref v 0))

(define (vy v)
  "Return the second component of the vector V."
  (vector-ref v 1))

(define (vz v)
  "Return the third component of the vector V."
  (vector-ref v 2))

(define (vw v)
  "Return the fourth component of the vector V."
  (vector-ref v 3))

(define (polar-vector r theta)
  "Create a 2D cartesian vector from the polar coordinates (R,
THETA)."
  (vector (* r (cos theta))
          (* r (sin theta))))

(define (dimension-error v1 v2)
  (error "Vector dimensionality mismatch: " v1 v2))

(define* (vreduce op vectors #:optional (reduce reduce))
  (reduce (lambda args
            (match args
              (((? number? k) (? number? l))
               (op k l))
              (((? number? k) (? vector? v))
               (vector-map (lambda (i n) (op k n)) v))
              (((? vector? v) (? number? k))
               (vector-map (lambda (i n) (op n k)) v))
              (((? vector? v1) (? vector? v2))
               (if (vector-length= v1 v2)
                   (vector-map (lambda (i a b)
                                 (op a b))
                               v1 v2)
                   (dimension-error v1 v2)))))
          0 vectors))

(define (v+ . vectors)
  "Return the sum of all vectors.  All vectors must be of the same
dimensionality.  Scalar values can be used to add to all components of
the resulting vector."
  (vreduce + vectors))

(define v-
  (case-lambda
    "Return the difference of all vectors.  All vectors must be of the
same dimensionality.  Scalar values can be used to subtract from all
components of the resulting vector."
    ((v) (v- 0 v))
    ((v . rest)
     (vreduce - (cons v rest) reduce-right))))

(define (v* . vectors)
  "Return the product of all VECTORS.  All vectors must be of the same
dimensionality.  Scalar values can be used to multiply all components
of the resulting vector."
  (vreduce * vectors))

(define (vdot v1 v2)
  "Return the dot product of the vectors V1 and V2.  V1 and V2 must be
of the same dimensionality."
  (if (vector-length= v1 v2)
      (vector-fold (lambda (i memo a b)
                     (+ memo (* a b)))
                   0 v1 v2)
      (dimension-error v1 v2)))

(define (vcross v1 v2)
  "Return the cross product of the vectors V1 and V2.  V1 and V2 must
both be 3D vectors."
  (match (list v1 v2)
    ((#(x1 y1 z1) #(x2 y2 z2))
     (vector (- (* y1 z2) (* z1 y2))
             (- (* z1 x2) (* x1 z2))
             (- (* x1 y2) (* y1 x2))))
    (_ (error "Expected 3D vectors: " v1 v2))))

(define (magnitude v)
  "Return the magnitude of the vector V."
  (sqrt
   (vector-fold (lambda (i memo n)
                  (+ memo (expt n 2)))
                0 v)))

(define (normalize v)
  "Normalize the vector V."
  (let ((m (magnitude v)))
    (if (zero? m)
        0
        (vector-map (lambda (i n)
                      (/ n m))
                    v))))
