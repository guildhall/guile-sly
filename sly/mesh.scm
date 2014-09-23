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
;; A mesh is a 2D/3D model comprised of a texture, shader, and vertex
;; buffers.
;;
;;; Code:

(define-module (sly mesh)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (system foreign)
  #:use-module (gl)
  #:use-module (gl low-level)
  #:use-module (sly wrappers gl)
  #:use-module (sly color)
  #:use-module (sly shader)
  #:use-module (sly texture)
  #:use-module (sly vector)
  #:use-module (sly signal)
  #:export (make-mesh
            mesh?
            mesh-length
            mesh-shader
            mesh-texture
            draw-mesh))

;;;
;;; Vertex Buffers and Vertex Arrays
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
     (cond
      ((number? elem)
       (lambda (i k)
         (setter bv i k)))
      ((or (vector2? elem)
           (vector3? elem)
           (vector4? elem))
       (let ((dimensions (vector-length elem)))
         (lambda (i v)
           (let ((offset (* i dimensions)))
             (vector-for-each
              (lambda (j n)
                (setter bv (+ offset j) n))
              v)))))
      ((color? elem)
       (lambda (i c)
         (let ((offset (* i 4)))
           (setter bv offset (color-r c))
           (setter bv (1+ offset) (color-g c))
           (setter bv (+ offset 2) (color-b c))
           (setter bv (+ offset 3) (color-a c))))))
     vertices)
    bv))

(define (attribute-size attr)
  (cond
   ((number? attr) 1)
   ((vector2? attr) 2)
   ((vector3? attr) 3)
   ((or (vector4? attr)
        (color? attr))
    4)
   (else
    (error "Unsupported attribute: " attr))))

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

(define (generate-vertex-array)
  (let ((bv (u32vector 1)))
    (glGenVertexArrays 1 (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define-syntax-rule (with-vertex-array vao body ...)
  (begin
    (glBindVertexArray vao)
    body ...
    (glBindVertexArray 0)))

(define (vertex-attrib-pointer shader attribute vbo)
  (let ((location (shader-program-attribute-location shader attribute)))
    (glEnableVertexAttribArray location)
    (with-vertex-buffer vbo
      (glVertexAttribPointer location (vertex-buffer-attr-size vbo)
                             (data-type float) #f 0 %null-pointer))))

;;;
;;; Mesh
;;;

(define-record-type <mesh>
  (%make-mesh vao length shader texture)
  mesh?
  (vao mesh-vao)
  (length mesh-length)
  (shader mesh-shader)
  (texture mesh-texture))

(define* (make-mesh #:optional #:key shader texture indices data)
  (let ((vao (generate-vertex-array)))
    (with-vertex-array vao
      (let loop ((data data))
        (match data
          (((attribute vertices) . rest)
           (vertex-attrib-pointer shader attribute
                                  (make-vertex-buffer vertices))
           (loop rest))
          (() #f)))
      (bind-vertex-buffer (make-vertex-buffer indices #t)))
    (%make-mesh vao (vector-length indices) shader texture)))

(define (draw-mesh mesh uniforms)
  (define (draw)
    (glDrawElements (begin-mode triangles)
                    (mesh-length mesh)
                    (data-type unsigned-int)
                    %null-pointer))

  (with-shader-program (mesh-shader mesh)
    (for-each (lambda (uniform)
                (match uniform
                  ((name value)
                   (uniform-set! (mesh-shader mesh) name
                                 (signal-ref-maybe value)))))
              uniforms)
    (with-vertex-array (mesh-vao mesh)
      (if (texture? (mesh-texture mesh))
          (with-texture (mesh-texture mesh) (draw))
          (draw)))))
