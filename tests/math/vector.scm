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

(define-module (test-vector)
  #:use-module (srfi srfi-64)
  #:use-module (sly math vector))

(test-begin "vector")

(test-group "vx"
  (test-equal 1 (vx (vector2 1 2)))
  (test-equal 1 (vx (vector3 1 2 3)))
  (test-equal 1 (vx (vector4 1 2 3 4))))

(test-group "vy"
  (test-equal 2 (vy (vector2 1 2)))
  (test-equal 2 (vy (vector3 1 2 3)))
  (test-equal 2 (vy (vector4 1 2 3 4))))

(test-group "vz"
  (test-equal 3 (vz (vector3 1 2 3)))
  (test-equal 3 (vz (vector4 1 2 3 4))))

(test-group "vw"
  (test-equal 4 (vw (vector4 1 2 3 4))))

(test-group "v+"
  (test-equal 0 (v+))
  (test-equal (vector2 1 1) (v+ (vector2 1 1)))
  (test-equal (vector2 9 12)
    (v+ (vector2 1 2) (vector2 3 4) (vector2 5 6)))
  (test-equal (vector2 8 10)
    (v+ (vector2 1 2) 3 (vector2 4 5)))
  (test-equal (vector3 12 15 18)
    (v+ (vector3 1 2 3) (vector3 4 5 6) (vector3 7 8 9)))
  (test-equal (vector3 10 12 14)
    (v+ (vector3 1 2 3) 4 (vector3 5 6 7)))
  (test-equal (vector4 15 18 21 24)
    (v+ (vector4 1 2 3 4) (vector4 5 6 7 8) (vector4 9 10 11 12)))
  (test-equal (vector4 12 14 16 18)
    (v+ (vector4 1 2 3 4) 5 (vector4 6 7 8 9))))

(test-group "v+"
  (test-equal 0 (v+))
  (test-equal (vector2 1 1) (v+ (vector2 1 1)))
  (test-equal (vector2 9 12)
    (v+ (vector2 1 2) (vector2 3 4) (vector2 5 6)))
  (test-equal (vector2 8 10)
    (v+ (vector2 1 2) 3 (vector2 4 5)))
  (test-equal (vector3 12 15 18)
    (v+ (vector3 1 2 3) (vector3 4 5 6) (vector3 7 8 9)))
  (test-equal (vector3 10 12 14)
    (v+ (vector3 1 2 3) 4 (vector3 5 6 7)))
  (test-equal (vector4 15 18 21 24)
    (v+ (vector4 1 2 3 4) (vector4 5 6 7 8) (vector4 9 10 11 12)))
  (test-equal (vector4 12 14 16 18)
    (v+ (vector4 1 2 3 4) 5 (vector4 6 7 8 9))))

(test-group "v-"
  (test-equal (vector2 -1 -1) (v- (vector2 1 1)))
  (test-equal (vector2 0 1)
    (v- (vector2 6 5) (vector2 4 3) (vector2 2 1)))
  (test-equal (vector2 0 0)
    (v- (vector2 5 4) 3 (vector2 2 1)))
  (test-equal (vector3 0 1 2)
    (v- (vector3 9 8 7) (vector3 6 5 4) (vector3 3 2 1)))
  (test-equal (vector3 0 0 0)
    (v- (vector3 7 6 5) 4 (vector3 3 2 1)))
  (test-equal (vector4 0 1 2 3)
    (v- (vector4 12 11 10 9) (vector4 8 7 6 5) (vector4 4 3 2 1)))
  (test-equal (vector4 0 0 0 0)
    (v- (vector4 9 8 7 6) 5 (vector4 4 3 2 1))))

(test-group "v*"
  (test-equal 1 (v*))
  (test-equal (vector2 1 1) (v* (vector2 1 1)))
  (test-equal (vector2 15 48)
    (v* (vector2 1 2) (vector2 3 4) (vector2 5 6)))
  (test-equal (vector2 12 30)
    (v* (vector2 1 2) 3 (vector2 4 5)))
  (test-equal (vector3 28 80 162)
    (v* (vector3 1 2 3) (vector3 4 5 6) (vector3 7 8 9)))
  (test-equal (vector3 20 48 84)
    (v* (vector3 1 2 3) 4 (vector3 5 6 7)))
  (test-equal (vector4 45 120 231 384)
    (v* (vector4 1 2 3 4) (vector4 5 6 7 8) (vector4 9 10 11 12)))
  (test-equal (vector4 30 70 120 180)
    (v* (vector4 1 2 3 4) 5 (vector4 6 7 8 9))))

(test-group "vdot"
  (test-equal 11 (vdot (vector2 1 2) (vector2 3 4)))
  (test-equal 32 (vdot (vector3 1 2 3) (vector3 4 5 6)))
  (test-equal 70 (vdot (vector4 1 2 3 4) (vector4 5 6 7 8))))

(test-group "vcross"
  (test-equal (vector3 -3 6 -3)
    (vcross (vector3 2 3 4) (vector3 5 6 7))))

(test-group "magnitude"
  (test-equal 5 (magnitude (vector2 3 4)))
  (test-equal 3 (magnitude (vector3 1 2 2)))
  (test-equal 4 (magnitude (vector4 2 2 2 2))))

(test-group "normalize"
  (test-equal (vector2 0 0) (normalize (vector2 0 0)))
  (test-equal (vector3 0 0 0) (normalize (vector3 0 0 0)))
  (test-equal (vector4 0 0 0 0) (normalize (vector4 0 0 0 0)))
  (test-equal (vector2 3/5 4/5) (normalize (vector2 3 4)))
  (test-equal (vector3 1/3 2/3 2/3) (normalize (vector3 1 2 2)))
  (test-equal (vector4 1/2 1/2 1/2 1/2) (normalize (vector4 2 2 2 2))))

(test-end "vector")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
