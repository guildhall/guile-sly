;;; guile-2d
;;; Copyright (C) 2014 David Thompson <dthompson2@worcester.edu>
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
;; 4x4 column-major transformation matrix.
;;
;;; Code:

(define-module (2d transform)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-42)
  #:use-module (2d math)
  #:use-module (2d vector2)
  #:export (make-transform
            make-transform*
            null-transform
            identity-transform
            transform?
            transform-matrix
            transpose
            transform-vector2
            transform-position
            transform+
            transform*
            transform-translate
            transform-scale
            transform-rotate
            orthographic-projection
            perspective-projection))

(define-record-type <transform>
  (%make-transform matrix)
  transform?
  (matrix transform-matrix))

(define (make-4x4-matrix)
  (make-typed-array 'f32 0 4 4))

(define (make-transform aa ab ac ad
                        ba bb bc bd
                        ca cb cc cd
                        da db dc dd)
  "Return a new transform initialized with the given 16 values in
column-major format."
  (let ((matrix (make-4x4-matrix)))
    (array-set! matrix aa 0 0)
    (array-set! matrix ab 0 1)
    (array-set! matrix ac 0 2)
    (array-set! matrix ad 0 3)
    (array-set! matrix ba 1 0)
    (array-set! matrix bb 1 1)
    (array-set! matrix bc 1 2)
    (array-set! matrix bd 1 3)
    (array-set! matrix ca 2 0)
    (array-set! matrix cb 2 1)
    (array-set! matrix cc 2 2)
    (array-set! matrix cd 2 3)
    (array-set! matrix da 3 0)
    (array-set! matrix db 3 1)
    (array-set! matrix dc 3 2)
    (array-set! matrix dd 3 3)
    (%make-transform matrix)))

(define* (make-transform* #:optional #:key (translate null-vector2)
                          (scale (vector2 1 1)) (rotate 0))
  "Return a new transform that is the result of the composition of the
given TRANSLATE, SCALE, and ROTATE values.  Both TRANSLATE and SCALE
are vector2 values, while ROTATE is a number."
  (transform* (transform-scale scale)
              (transform-rotate rotate)
              (transform-translate translate)))

(define null-transform
  (%make-transform (make-4x4-matrix)))

(define identity-transform
  (make-transform 1 0 0 0
                  0 1 0 0
                  0 0 1 0
                  0 0 0 1))

(define (transpose transform)
  "Return a transform that is the transpose of TRANSFORM."
  (let ((m1 (transform-matrix transform))
        (m2 (make-4x4-matrix)))
    (do-ec (: r 4) (: c 4)
           (array-set! m2 (array-ref m1 r c)
                       c r))
    (%make-transform m2)))

(define (transform-vector2 transform v)
  "Apply TRANSFORM to the vector2 V."
  (let ((m (transform-matrix transform))
        (x (vx v))
        (y (vy v)))
    (vector2 (+ (* x (array-ref m 0 0))
                (* y (array-ref m 0 1))
                (array-ref m 0 3))
             (+ (* x (array-ref m 1 0))
                (* y (array-ref m 1 1))
                (array-ref m 1 3)))))

(define (transform-position transform)
  "Extract 2D vector from TRANSFORM."
  (let ((m (transform-matrix transform)))
    (vector2 (array-ref m 0 3) (array-ref m 1 3))))

(define (transform+ . transforms)
  "Return the sum of all given transformation matrices.  Return
null-transform if called without any arguments."
  (define (add a b)
    (let ((m1 (transform-matrix a))
          (m2 (transform-matrix b))
          (m3 (make-4x4-matrix)))
      (do-ec (: r 4) (: c 4)
             (let ((x (+ (array-ref m1 r c)
                         (array-ref m2 r c))))
               (array-set! m3 x r c)))
      (%make-transform m3)))
  (reduce add null-transform transforms))

(define (transform* . transforms)
  "Return the product of all given transformation matrices.  Return
identity-transform if called without any arguments."
  (define (mul a b)
    (let ((m1 (transform-matrix a))
          (m2 (transform-matrix b))
          (m3 (make-4x4-matrix)))
      (do-ec (: r 4) (: c 4)
             (let ((x (sum-ec (: k 4)
                              (* (array-ref m1 r k)
                                 (array-ref m2 k c)))))
               (array-set! m3 x r c)))
      (%make-transform m3)))
  (reduce mul identity-transform transforms))

(define (transform-translate v)
  "Return a new transform that translates the x and y axes by the
vector2 V."
  (make-transform 1      0      0 0
                  0      1      0 0
                  0      0      1 0
                  (vx v) (vy v) 0 1))

(define (transform-scale v)
  "Return a new transform that scales the X and Y axes by the vector2
V."
  (make-transform (vx v) 0      0 0
                  0      (vy v) 0 0
                  0      0      1 0
                  0      0      0 1))

(define (transform-rotate angle)
  "Return a new transform that rotates the Z axis by ANGLE radians."
  (make-transform (cos angle) (- (sin angle)) 0 0
                  (sin angle) (cos angle)     0 0
                  0           0               1 0
                  0           0               0 1))

(define (orthographic-projection left right top bottom near far)
  "Return a new transform that represents an orthographic projection
for the vertical clipping plane LEFT and RIGHT, the horizontal
clipping plane TOP and BOTTOM, and the depth clipping plane NEAR and
FAR."
  (make-transform (/ 2 (- right left)) 0 0 0
                  0 (/ 2 (- top bottom)) 0 0
                  0 0 (/ 2 (- far near)) 0
                  (- (/ (+ right left) (- right left)))
                  (- (/ (+ top bottom) (- top bottom)))
                  (- (/ (+ far near) (- far near)))
                  1))

(define (perspective-projection field-of-vision aspect-ratio near far)
  "Return a new transform that represents a perspective projection
with a FIELD-OF-VISION in degrees, the desired ASPECT-RATIO, and the
depth clipping plane NEAR and FAR."
  (let ((size (* near (tan (/ (degrees->radians field-of-vision) 2)))))
    (let ((left (- size))
          (right size)
          (top (/ size aspect-ratio))
          (bottom (/ (- size) aspect-ratio)))
      (make-transform (/ (* 2 near) (- right left)) ;; First row
                      0
                      (/ (+ right left) (- right left))
                      0
                      ;; Second row
                      0
                      (/ (* 2 near) (- top bottom))
                      (/ (+ top bottom) (- top bottom))
                      0
                      ;; Third row
                      0 0
                      (- (/ (+ far near) (- far near)))
                      (- (/ (* 2 far near) (- far near)))
                      ;; Fourth row
                      0 0 -1 0))))
