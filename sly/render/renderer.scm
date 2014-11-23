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
;; OpenGL renderer.
;;
;;; Code:

(define-module (sly render renderer)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (gl)
  #:use-module (gl low-level)
  #:use-module (sly render shader)
  #:use-module (sly render texture)
  #:use-module (sly math transform)
  #:use-module (sly math vector)
  #:use-module (sly render utils)
  #:use-module (sly render camera)
  #:use-module (sly render context)
  #:use-module (sly render vertex-array)
  #:export (make-render-op render-op?
            render-op-transform render-op-vertex-array
            render-op-texture render-op-shader
            render-op-blend-mode render-op-uniforms
            transform-render-op
            make-renderer renderer?
            renderer-ops
            render))

;; Representation of a single OpenGL render call.
(define-record-type <render-op>
  (%make-render-op transform vertex-array texture shader uniforms
                   blend-mode depth-test?)
  render-op?
  (transform render-op-transform)
  (vertex-array render-op-vertex-array)
  (texture render-op-texture)
  (shader render-op-shader)
  (uniforms render-op-uniforms)
  (blend-mode render-op-blend-mode)
  (depth-test? render-op-depth-test?))

(define* (make-render-op #:optional #:key (transform identity-transform)
                         (vertex-array #f) (texture #f) (shader #f)
                         (uniforms '()) (blend-mode default-blend-mode)
                         (depth-test? #t))
  "Create a new render operation object.  Optional arguments include:
TRANSFORM, a model transformation matrix.  VERTEX-ARRAY, the geometry
container.  TEXTURE, the texture object to bind.  SHADER, the shader
program to bind.  UNIFORMS, the variables to be passed to the shader.
And DEPTH-TEST?, a flag that determines whether the depth buffer is
activated or not."
  (%make-render-op transform vertex-array texture shader uniforms
                   blend-mode depth-test?))

(define* (transform-render-op op transform)
  "Return a new render operation object that is the same as OP, but
with its transformation matrix multiplied by TRANSFORM."
  (match op
    (($ <render-op> local-transform vertex-array texture shader uniforms
                    blend-mode depth-test?)
     (%make-render-op (transform* transform local-transform) vertex-array
                      texture shader uniforms blend-mode depth-test?))))

(define (apply-render-op context view op)
  "Render OP by applying its transform (multiplied by VIEW), texture,
shader, vertex array, uniforms, blend mode, etc. to the render
CONTEXT."
  (match op
    (($ <render-op> transform vertex-array texture shader uniforms
                    blend-mode depth-test?)
     (set-render-context-depth-test?! context depth-test?)
     (set-render-context-blend-mode! context blend-mode)
     (set-render-context-shader! context shader)
     (set-render-context-vertex-array! context vertex-array)
     (set-render-context-texture! context texture)
     (for-each (lambda (uniform)
                 (match uniform
                   ((name value)
                    (uniform-set! shader name value))))
               `(("mvp" ,(transform* view transform))
                 ,@uniforms))
     (glDrawElements (begin-mode triangles)
                     (vertex-array-length vertex-array)
                     (data-type unsigned-int)
                     %null-pointer))))

(define-record-type <renderer>
  (make-renderer context cameras ops)
  renderer?
  (context renderer-context)
  (cameras renderer-cameras)
  (ops renderer-ops))

(define (render renderer)
  "Apply all of the render operations in RENDERER.  The render
operations are applied once for each camera."
  (let ((context (renderer-context renderer)))
    (with-render-context context
      (for-each (lambda (camera)
                  (let ((view (transform* (camera-projection camera)
                                          (camera-location camera))))
                    (for-each (cut apply-render-op context view <>)
                              (renderer-ops renderer))))
                (renderer-cameras renderer)))))
