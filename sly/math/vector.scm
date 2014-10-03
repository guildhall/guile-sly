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

(define-module (sly math vector)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (sly math)
  #:export (<vector2>
            <vector3>
            <vector4>
            vector2 vector3 vector4
            vector2? vector3? vector4?
            vx vy vz vw
            v+ v- v* vdot vcross
            magnitude normalize vlerp))

(define-record-type <vector2>
  (vector2 x y)
  vector2?
  (x vector2-x)
  (y vector2-y))

(define-record-type <vector3>
  (vector3 x y z)
  vector3?
  (x vector3-x)
  (y vector3-y)
  (z vector3-z))

(define-record-type <vector4>
  (vector4 x y z w)
  vector4?
  (x vector4-x)
  (y vector4-y)
  (z vector4-z)
  (w vector4-w))

(define vx
  (match-lambda
   ((or ($ <vector2> x _)
        ($ <vector3> x _ _)
        ($ <vector4> x _ _ _))
    x)))

(define vy
  (match-lambda
   ((or ($ <vector2> _ y)
        ($ <vector3> _ y _)
        ($ <vector4> _ y _ _))
    y)))

(define vz
  (match-lambda
   ((or ($ <vector3> _ _ z)
        ($ <vector4> _ _ z _))
    z)))

(define vw vector4-w)

(define-syntax-rule (vector-lambda proc)
  (match-lambda*
   ((($ <vector2> x1 y1) ($ <vector2> x2 y2))
    (vector2 (proc x1 x2) (proc y1 y2)))
   ((($ <vector2> x y) (? number? k))
    (vector2 (proc x k) (proc y k)))
   (((? number? k) ($ <vector2> x y))
    (vector2 (proc k x) (proc k y)))
   ((($ <vector3> x1 y1 z1) ($ <vector3> x2 y2 z2))
    (vector3 (proc x1 x2) (proc y1 y2) (proc z1 z2)))
   ((($ <vector3> x y z) (? number? k))
    (vector3 (proc x k) (proc y k) (proc z k)))
   (((? number? k) ($ <vector3> x y z))
    (vector3 (proc k x) (proc k y) (proc k z)))
   ((($ <vector4> x1 y1 z1 w1) ($ <vector4> x2 y2 z2 w2))
    (vector4 (proc x1 x2) (proc y1 y2) (proc z1 z2) (proc w1 w2)))
   ((($ <vector4> x y z w) (? number? k))
    (vector4 (proc x k) (proc y k) (proc z k) (proc w k)))
   (((? number? k) ($ <vector4> x y z w))
    (vector4 (proc k x) (proc k y) (proc k z) (proc k w)))))

(define (v+ . vectors)
  (reduce (vector-lambda +) 0 vectors))

(define v-
  (match-lambda*
   ((v) (v- 0 v))
   ((v v* ...)
    (fold-right (let ((- (vector-lambda -)))
                  (lambda (prev v)
                    (- v prev)))
                v v*))))

(define (v* . vectors)
  (reduce (vector-lambda *) 1 vectors))

(define vdot
  (match-lambda*
   ((($ <vector2> x1 y1) ($ <vector2> x2 y2))
    (+ (* x1 x2) (* y1 y2)))
   ((($ <vector3> x1 y1 z1) ($ <vector3> x2 y2 z2))
    (+ (* x1 x2) (* y1 y2) (* z1 z2)))
   ((($ <vector4> x1 y1 z1 w1) ($ <vector4> x2 y2 z2 w2))
    (+ (* x1 x2) (* y1 y2) (* z1 z2) (* w1 w2)))))

(define vcross
  (match-lambda*
   ((($ <vector3> x1 y1 z1) ($ <vector3> x2 y2 z2))
    (vector3 (- (* y1 z2) (* z1 y2))
             (- (* z1 x2) (* x1 z2))
             (- (* x1 y2) (* y1 x2))))))

(define (magnitude v)
  "Return the magnitude of the vector V."
  (sqrt
   (match v
     (($ <vector2> x y)
      (+ (square x) (square y)))
     (($ <vector3> x y z)
      (+ (square x) (square y) (square z)))
     (($ <vector4> x y z w)
      (+ (square x) (square y) (square z) (square w))))))

(define (normalize v)
  "Return the normalized form of the vector V."
  (let ((m (magnitude v)))
    (if (zero? m)
        v
        (match v
          (($ <vector2> x y)
           (vector2 (/ x m) (/ y m)))
          (($ <vector3> x y z)
           (vector3 (/ x m) (/ y m) (/ z m)))
          (($ <vector4> x y z w)
           (vector4 (/ x m) (/ y m) (/ z m) (/ w m)))))))

(define vlerp (make-lerp v+ v*))
