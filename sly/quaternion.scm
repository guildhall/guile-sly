;;; Sly
;;; Copyright (C) 2013, 2014 David Thompson <dthompson2@worcester.edu>
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
;; Useful representation of 3D rotations.
;;
;;; Code:

(define-module (sly quaternion)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (sly math)
  #:use-module (sly math vector)
  #:export (<quaternion> make-quaternion quaternion
            quaternion?
            quaternion-w quaternion-x quaternion-y quaternion-z
            identity-quaternion null-quaternion
            quaternion* quaternion-slerp
            quaternion-magnitude quaternion-normalize
            vector->quaternion quaternion->vector))

(define-record-type <quaternion>
  (%make-quaternion w x y z)
  quaternion?
  (w quaternion-w)
  (x quaternion-x)
  (y quaternion-y)
  (z quaternion-z))

(define make-quaternion
  (match-lambda*
   ((($ <vector3> x y z) (? number? theta))
    ;; Convert an axis angle to a quaternion
    (let* ((theta/2 (/ theta 2))
           (sin (sin theta/2)))
      (%make-quaternion (cos theta/2) (* x sin) (* y sin) (* z sin))))
   ((w x y z)
    (%make-quaternion w x y z))))

(define quaternion make-quaternion)

(define identity-quaternion (make-quaternion 1 0 0 0))
(define null-quaternion (make-quaternion 0 0 0 0))

(define (quaternion* . quaternions)
  "Return the product of all QUATERNIONS.  If called without
arguments, 'identity-quaternion' is returned."
  (reduce (lambda args
            (match args
              ((($ <quaternion> w1 x1 y1 z1) ($ <quaternion> w2 x2 y2 z2))
               (make-quaternion
                (- (* w1 w2) (* x1 x2) (* y1 y2) (* z1 z2))
                (+ (* w1 x2) (* x1 w2) (* y1 z2) (- (* z1 y2)))
                (+ (* w1 y2) (* y1 w2) (* z1 x2) (- (* x1 z2)))
                (+ (* w1 z2) (* z1 w2) (* x1 y2) (- (* y1 x2)))))))
          identity-quaternion
          quaternions))

(define (quaternion-slerp q1 q2 delta)
  "Perform a spherical linear interpolation of the quaternions Q1 and
Q2 and blending factor DELTA."
  (let* ((q1 (quaternion->vector q1))
         (q2 (quaternion->vector q2))
         (dot (clamp -1 1 (vdot q1 q2)))
         (theta (* (acos dot) delta))
         (q3 (normalize (v- q2 (v* q1 dot)))))
    (vector->quaternion
     (v+ (v* q1 (cos theta)) (v* q3 (sin theta))))))

(define (quaternion-magnitude q)
  "Return the magnitude of the quaternion Q."
  (sqrt
   (+ (square (quaternion-w q))
      (square (quaternion-x q))
      (square (quaternion-y q))
      (square (quaternion-z q)))))

(define (quaternion-normalize q)
  "Return the normalized form of the quaternion Q."
  (let ((m (quaternion-magnitude q)))
    (if (zero? m)
        (make-quaternion 0 0 0 0)
        (make-quaternion (/ (quaternion-w q) m)
                         (/ (quaternion-x q) m)
                         (/ (quaternion-y q) m)
                         (/ (quaternion-z q) m)))))

(define quaternion->vector
  (match-lambda
    (($ <quaternion> w x y z)
     (vector4 w x y z))))

(define vector->quaternion
  (match-lambda
    (($ <vector4> x y z w)
     (make-quaternion x y z w))))
