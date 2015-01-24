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
;; Meshes encapsulate the geometry for a single OpenGL draw call.
;;
;;; Code:

(define-module (sly render mesh)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (gl)
  #:use-module (gl low-level)
  #:use-module (sly utils)
  #:use-module (sly wrappers gl)
  #:use-module (sly math vector)
  #:use-module (sly render color)
  #:use-module (sly render shader)
  #:export (make-mesh
            mesh?
            mesh-id mesh-length
            apply-mesh with-mesh))

;;;
;;; Vertex Buffer
;;;

(define-record-type <vertex-buffer>
  (%make-vertex-buffer id type usage data)
  vertex-buffer?
  (id vertex-buffer-id)
  (type vertex-buffer-type)
  (usage vertex-buffer-usage)
  (data vertex-buffer-data))

(define (generate-vertex-buffer)
  (let ((bv (u32vector 1)))
    (glGenBuffers 1 (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define (bind-vertex-buffer vbo)
  (glBindBuffer (vertex-buffer-target vbo)
                (vertex-buffer-id vbo)))

(define-syntax-rule (with-vertex-buffer vbo body ...)
  (begin
    (bind-vertex-buffer vbo)
    body ...))

(define attribute-type
  (match-lambda
   ((? number? _) 'float)
   ((? vector2? _) 'vec2)
   ((? vector3? _) 'vec3)
   ((or (? vector4? _)
        (? color? _))
    'vec4)
   (attr
    (error "Unsupported vertex buffer attribute: " attr))))

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

(define (vertex-buffer-attribute-size vbo)
  (match (vertex-buffer-type vbo)
    ((or 'float 'index) 1)
    ('vec2 2)
    ('vec3 3)
    ('vec4 4)))

(define (index-buffer? vbo)
  (eq? (vertex-buffer-type vbo) 'index))

(define (vertex-buffer-target vbo)
  (if (index-buffer? vbo)
      (arb-vertex-buffer-object element-array-buffer-arb)
      (arb-vertex-buffer-object array-buffer-arb)))

(define (vertex-buffer-length vbo)
  (/ (bytevector-length (vertex-buffer-data vbo))
     (* (vertex-buffer-attribute-size vbo) 4)))

(define (vertex-buffer-usage-gl vbo)
  (match (vertex-buffer-usage vbo)
    ('static
     (arb-vertex-buffer-object static-draw-arb))
    ('stream
     (arb-vertex-buffer-object stream-draw-arb))))

(define (make-vertex-buffer* type usage data)
  (let ((vbo (%make-vertex-buffer (generate-vertex-buffer) type usage data)))
    (with-vertex-buffer vbo
      (glBufferData (vertex-buffer-target vbo)
                    (bytevector-length data)
                    (bytevector->pointer data)
                    (vertex-buffer-usage-gl vbo)))
    vbo))

(define* (make-vertex-buffer vertices #:optional (index? #f) (usage 'static))
  (let ((data (vertices-bytevector vertices index?)))
    (make-vertex-buffer* (if index?
                             'index
                             (attribute-type (vector-ref vertices 0)))
                         usage
                         data)))

;;;
;;; Mesh
;;;

(define-record-type <mesh>
  (%make-mesh id length vbos)
  mesh?
  (id mesh-id)
  (length mesh-length)
  (vbos mesh-vbos))

(define-guardian mesh-guardian
  (lambda (mesh)
    ;; Delete vertex array and vertex buffers.
    (glDeleteVertexArrays 1 (u32vector (mesh-id mesh)))
    (let ((buffers (mesh-vbos mesh)))
      (glDeleteBuffers (length buffers)
                       (list->u32vector (map vertex-buffer-id buffers))))))

(define (generate-vertex-array)
  (let ((bv (u32vector 1)))
    (glGenVertexArrays 1 (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define (apply-mesh vao)
  (glBindVertexArray (mesh-id vao)))

;; emacs: (put 'with-mesh 'scheme-indent-function 1)
(define-syntax-rule (with-mesh vao body ...)
  (begin
    (apply-mesh vao)
    body ...
    (glBindVertexArray 0)))

(define (vertex-attrib-pointer location vbo)
  (glEnableVertexAttribArray location)
  (with-vertex-buffer vbo
    (glVertexAttribPointer location (vertex-buffer-attribute-size vbo)
                           (data-type float) #f 0 %null-pointer)))

(define (make-mesh indices positions textures)
  (let* ((index-buffer (make-vertex-buffer indices #t))
         (position-buffer (make-vertex-buffer positions))
         (texture-buffer (make-vertex-buffer textures))
         (mesh (%make-mesh (generate-vertex-array)
                           (vector-length indices)
                           (list index-buffer
                                 position-buffer
                                 texture-buffer))))
    (with-mesh mesh
      (vertex-attrib-pointer vertex-position-location position-buffer)
      (if textures
          (vertex-attrib-pointer vertex-texture-location texture-buffer))
      (bind-vertex-buffer index-buffer))
    (mesh-guardian mesh)
    mesh))
