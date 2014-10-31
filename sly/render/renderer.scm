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
  #:use-module (sly shader)
  #:use-module (sly texture)
  #:use-module (sly transform)
  #:use-module (sly math vector)
  #:use-module (sly render utils)
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

(define-syntax-rule (with-texture-maybe texture body ...)
  (if texture
      (with-texture texture body ...)
      (begin body ...)))

(define (apply-render-op op)
  "Render OP by applying its texture, shader, vertex array, uniforms,
blend mode, etc.."
  (match op
    (($ <render-op> transform vertex-array texture shader uniforms
                    blend-mode depth-test?)
     (when depth-test?
       (gl-enable (enable-cap depth-test)))
     (if blend-mode
         (begin
           (gl-enable (enable-cap blend))
           (apply-blend-mode blend-mode))
         (gl-disable (enable-cap blend)))
     (with-shader-program shader
       (for-each (lambda (uniform)
                   (match uniform
                     ((name value)
                      (uniform-set! shader name value))))
                 `(("mvp" ,transform)
                   ,@uniforms))
       (with-vertex-array vertex-array
         (with-texture-maybe texture
           (glDrawElements (begin-mode triangles)
                           (vertex-array-length vertex-array)
                           (data-type unsigned-int)
                           %null-pointer))))
     (when depth-test?
       (gl-disable (enable-cap depth-test))))))

(define-record-type <renderer>
  (make-renderer ops)
  renderer?
  (ops renderer-ops))

(define (render renderer)
  "Apply all of the render operations in RENDERER.  The render
operations are applied once for each camera."
  (for-each (cut apply-render-op <>)
            (renderer-ops renderer)))
