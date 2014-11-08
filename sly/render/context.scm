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
;; Manages OpenGL state and reduces state changes.
;;
;;; Code:

(define-module (sly render context)
  #:use-module (srfi srfi-9)
  #:use-module (gl)
  #:use-module (gl enums)
  #:use-module (gl low-level)
  #:use-module (sly wrappers gl)
  #:use-module (sly render shader)
  #:use-module (sly render texture)
  #:use-module (sly render utils)
  #:use-module (sly render vertex-array)
  #:export (make-render-context
            render-context?
            with-render-context
            render-context-blend-mode set-render-context-blend-mode!
            render-context-depth-test? set-render-context-depth-test?!
            render-context-texture set-render-context-texture!
            render-context-shader set-render-context-shader!
            render-context-vertex-array set-render-context-vertex-array!))

(define-record-type <render-context>
  (%make-render-context blend-mode depth-test? texture shader vertex-array)
  render-context?
  (blend-mode render-context-blend-mode %set-render-context-blend-mode!)
  (depth-test? render-context-depth-test? %set-render-context-depth-test?!)
  (texture render-context-texture %set-render-context-texture!)
  (shader render-context-shader %set-render-context-shader!)
  (vertex-array render-context-vertex-array %set-render-context-vertex-array!))

(define (make-render-context)
  (%make-render-context #f #f #f #f #f))

(define (render-context-reset! context)
  (gl-disable (enable-cap blend))
  (%set-render-context-blend-mode! context #f)
  (gl-disable (enable-cap depth-test))
  (%set-render-context-depth-test?! context #f)
  (glBindTexture (texture-target texture-2d) 0)
  (%set-render-context-texture! context #f)
  (glUseProgram 0)
  (%set-render-context-shader! context #f)
  (glBindVertexArray 0)
  (%set-render-context-vertex-array! context #f))

(define-syntax-rule (with-render-context context body ...)
  (begin (render-context-reset! context) body ...))

(define (set-render-context-blend-mode! context blend-mode)
  (unless (equal? (render-context-blend-mode context) blend-mode)
    (if blend-mode
        (apply-blend-mode blend-mode)
        ;; Turn off blending if there is no blend-mode specified.
        (gl-disable (enable-cap blend)))
    (%set-render-context-blend-mode! context blend-mode)))

(define (set-render-context-depth-test?! context depth-test?)
  (unless (eq? (render-context-depth-test? context) depth-test?)
    (if depth-test?
        (gl-enable (enable-cap depth-test))
        (gl-disable (enable-cap depth-test)))
    (%set-render-context-depth-test?! context depth-test?)))

(define (set-render-context-texture! context texture)
  (let ((current-texture (render-context-texture context)))
    (unless (equal? current-texture texture)
      (if texture
          (begin
            ;; Enable texturing if it was disabled.
            (unless current-texture
              (gl-enable (enable-cap texture-2d)))
            (apply-texture texture))
          (gl-disable (enable-cap texture-2d)))
      (%set-render-context-texture! context texture))))

(define (set-render-context-shader! context shader)
  (unless (equal? (render-context-shader context) shader)
    (apply-shader-program shader)
    (%set-render-context-shader! context shader)))

(define (set-render-context-vertex-array! context vertex-array)
  (unless (equal? (render-context-vertex-array context) vertex-array)
    (apply-vertex-array vertex-array)
    (%set-render-context-vertex-array! context vertex-array)))
