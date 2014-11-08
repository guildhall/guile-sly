;;; Sly
;;; Copyright (C) 2014 David Thompson <davet@gnu.org>
;;;
;;; Sly is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Sly is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Vertex arrays encapsulate the geometry for a single OpenGL draw
;; call.
;;
;;; Code:

(define-module (sly render vertex-array)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (gl)
  #:use-module (gl low-level)
  #:use-module (sly wrappers gl)
  #:use-module (sly math vector)
  #:use-module (sly render color)
  #:use-module (sly render shader)
  #:export (make-vertex-array
            vertex-array?
            vertex-array-id vertex-array-length
            apply-vertex-array with-vertex-array))

;;;
;;; Vertex Buffers
;;;

(define-record-type <vertex-buffer>
  (%make-vertex-buffer id type attr-size length)
  vertex-buffer?
  (id vertex-buffer-id)
  (type vertex-buffer-type)
  (attr-size vertex-buffer-attr-size)
  (length vertex-buffer-length))

(define (generate-vertex-buffer)
  (let ((bv (u32vector 1)))
    (glGenBuffers 1 (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define (bind-vertex-buffer vbo)
  (glBindBuffer (vertex-buffer-type vbo)
                (vertex-buffer-id vbo)))

(define-syntax-rule (with-vertex-buffer vbo body ...)
  (let ((type (vertex-buffer-type vbo)))
    (glBindBuffer type (vertex-buffer-id vbo))
    body ...
    (glBindBuffer type 0)))

(define (vertices-bytevector vertices index?)
  (let* ((elem (vector-ref vertices 0))
         (bv (if index?
                 (make-u32vector (vector-length vertices))
                 (make-f32vector (* (vector-length vertices)
                                    (attribute-size elem)))))
         (setter (if index? u32vector-set! f32vector-set!)))
    (vector-for-each
     (match-lambda*
      ((i (? number? k))
       (setter bv i k))
      ((i ($ <vector2> x y))
       (let ((offset (* i 2)))
         (setter bv offset x)
         (setter bv (1+ offset) y)))
      ((i ($ <vector3> x y z))
       (let ((offset (* i 3)))
         (setter bv offset x)
         (setter bv (1+ offset) y)
         (setter bv (+ offset 2) z)))
      ((i ($ <vector4> x y z w))
       (let ((offset (* i 4)))
         (setter bv offset x)
         (setter bv (1+ offset) y)
         (setter bv (+ offset 2) z)
         (setter bv (+ offset 3) w)))
      ((i (color? c))
       (let ((offset (* i 4)))
         (setter bv offset (color-r c))
         (setter bv (1+ offset) (color-g c))
         (setter bv (+ offset 2) (color-b c))
         (setter bv (+ offset 3) (color-a c)))))
     vertices)
    bv))

(define attribute-size
  (match-lambda
   ((? number? _) 1)
   ((? vector2? _) 2)
   ((? vector3? _) 3)
   ((or (? vector4? _)
        (? color? _))
    4)
   (attr
    (error "Unsupported vertex buffer attribute: " attr))))

(define (gl-buffer-type index?)
  (if index?
      (arb-vertex-buffer-object element-array-buffer-arb)
      (arb-vertex-buffer-object array-buffer-arb)))

(define* (make-vertex-buffer vertices #:optional (index? #f))
  (let ((bv (vertices-bytevector vertices index?))
        (vbo (%make-vertex-buffer (generate-vertex-buffer)
                                  (gl-buffer-type index?)
                                  (attribute-size (vector-ref vertices 0))
                                  (vector-length vertices))))
    (with-vertex-buffer vbo
      (glBufferData (vertex-buffer-type vbo)
                    (bytevector-length bv)
                    (bytevector->pointer bv)
                    (arb-vertex-buffer-object static-draw-arb)))
    vbo))

;;;
;;; Vertex Arrays
;;;

(define-record-type <vertex-array>
  (%make-vertex-array id length)
  vertex-array?
  (id vertex-array-id)
  (length vertex-array-length))

(define (generate-vertex-array)
  (let ((bv (u32vector 1)))
    (glGenVertexArrays 1 (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define (apply-vertex-array vao)
  (glBindVertexArray (vertex-array-id vao)))

(define-syntax-rule (with-vertex-array vao body ...)
  (begin
    (apply-vertex-array vao)
    body ...
    (glBindVertexArray 0)))

(define (vertex-attrib-pointer location vbo)
  (glEnableVertexAttribArray location)
  (with-vertex-buffer vbo
    (glVertexAttribPointer location (vertex-buffer-attr-size vbo)
                           (data-type float) #f 0 %null-pointer)))

(define (make-vertex-array indices positions textures)
  (let ((vao (%make-vertex-array (generate-vertex-array)
                                 (vector-length indices)))
        (positions (make-vertex-buffer positions))
        (textures (make-vertex-buffer textures)))
    (with-vertex-array vao
      (vertex-attrib-pointer vertex-position-location positions)
      (if textures
          (vertex-attrib-pointer vertex-texture-location textures))
      (bind-vertex-buffer (make-vertex-buffer indices #t)))
    vao))
