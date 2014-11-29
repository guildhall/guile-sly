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
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-42)
  #:use-module (gl)
  #:use-module (gl enums)
  #:use-module (gl low-level)
  #:use-module (sly wrappers gl)
  #:use-module (sly math transform)
  #:use-module (sly render shader)
  #:use-module (sly render texture)
  #:use-module (sly render utils)
  #:use-module (sly render mesh)
  #:export (make-render-context
            render-context?
            with-render-context with-temp-transform
            render-context-blend-mode set-render-context-blend-mode!
            render-context-depth-test? set-render-context-depth-test?!
            render-context-texture set-render-context-texture!
            render-context-shader set-render-context-shader!
            render-context-mesh set-render-context-mesh!))

(define-record-type <render-context>
  (%make-render-context blend-mode depth-test? texture shader
                        mesh transform-stack)
  render-context?
  (blend-mode render-context-blend-mode %set-render-context-blend-mode!)
  (depth-test? render-context-depth-test? %set-render-context-depth-test?!)
  (texture render-context-texture %set-render-context-texture!)
  (shader render-context-shader %set-render-context-shader!)
  (mesh render-context-mesh %set-render-context-mesh!)
  (transform-stack render-context-transform-stack))

(define (make-null-transform)
  (make-transform 0 0 0 0
                  0 0 0 0
                  0 0 0 0
                  0 0 0 0))

(define (make-transform-stack size)
  (let ((stack (make-q)))
    (do-ec (: i 128) (q-push! stack (make-null-transform)))
    stack))

(define* (make-render-context #:optional (transform-stack-size 32))
  (%make-render-context #f #f #f #f #f
                        (make-transform-stack transform-stack-size)))

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
  (%set-render-context-mesh! context #f))

(define-syntax-rule (with-render-context context body ...)
  (begin (render-context-reset! context) body ...))

(define (set-render-context-blend-mode! context blend-mode)
  (unless (equal? (render-context-blend-mode context) blend-mode)
    (if blend-mode
        (begin
          (gl-enable (enable-cap blend))
          (apply-blend-mode blend-mode))
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

(define (set-render-context-mesh! context mesh)
  (unless (equal? (render-context-mesh context) mesh)
    (apply-mesh mesh)
    (%set-render-context-mesh! context mesh)))

;; emacs: (put 'with-temp-transform 'scheme-indent-function 2)
(define-syntax-rule (with-temp-transform context name body ...)
  (let* ((stack (render-context-transform-stack context))
         (name (q-pop! stack)))
    (begin body ...)
    (q-push! stack name)))
