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
  #:use-module (ice-9 match)
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
  #:use-module (sly render framebuffer)
  #:use-module (sly render camera)
  #:export (make-render-context
            render-context?
            with-render-context
            render-context-blend-mode set-render-context-blend-mode!
            render-context-depth-test? set-render-context-depth-test?!
            render-context-texture set-render-context-texture!
            render-context-shader set-render-context-shader!
            render-context-mesh set-render-context-mesh!
            render-context-framebuffer set-render-context-framebuffer!
            render-context-viewport set-render-context-viewport!
            render-context-transform render-context-transform*!
            render-context-transform-identity!
            with-transform-excursion))

(define-record-type <gl-parameter>
  (%make-gl-parameter default bind value)
  gl-parameter?
  (default gl-parameter-default)
  (bind gl-parameter-bind)
  (value gl-parameter-ref %gl-parameter-set!))

(define (make-gl-parameter default bind)
  (%make-gl-parameter default bind default))

(define* (gl-parameter-set! parameter value #:optional force?)
  (unless (and (not force?) (equal? (gl-parameter-ref parameter) value))
    (%gl-parameter-set! parameter value)
    ((gl-parameter-bind parameter) value)))

(define (gl-parameter-reset! parameter)
  (gl-parameter-set! parameter (gl-parameter-default parameter) #t))

(define-record-type <render-context>
  (%make-render-context blend-mode depth-test? texture shader
                        mesh framebuffer viewport transform-stack)
  render-context?
  (blend-mode render-context-blend-mode)
  (depth-test? render-context-depth-test?)
  (texture render-context-texture)
  (shader render-context-shader)
  (mesh render-context-mesh)
  (framebuffer render-context-framebuffer)
  (viewport render-context-viewport)
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
  (%make-render-context (make-gl-parameter #f apply-blend-mode)
                        (make-gl-parameter #t apply-depth-test)
                        (make-gl-parameter null-texture apply-texture)
                        (make-gl-parameter null-shader-program
                                           apply-shader-program)
                        (make-gl-parameter null-mesh apply-mesh)
                        (make-gl-parameter null-framebuffer apply-framebuffer)
                        (make-gl-parameter null-viewport apply-viewport)
                        (make-transform-stack transform-stack-size)))

(define (render-context-reset! context)
  (gl-parameter-reset! (render-context-blend-mode context))
  (gl-parameter-reset! (render-context-depth-test? context))
  (gl-parameter-reset! (render-context-texture context))
  (gl-parameter-reset! (render-context-shader context))
  (gl-parameter-reset! (render-context-mesh context))
  (gl-parameter-reset! (render-context-framebuffer context))
  (gl-parameter-reset! (render-context-viewport context))
  (render-context-transform-identity! context))

(define-syntax-rule (with-render-context context body ...)
  (begin (render-context-reset! context)
         body ...
         (render-context-reset! context)))

(define-syntax-rule (define-context-setter name accessor)
  (define (name context value)
    (gl-parameter-set! (accessor context) value)))

(define-context-setter set-render-context-blend-mode!
  render-context-blend-mode)

(define-context-setter set-render-context-depth-test?!
  render-context-depth-test?)

(define-context-setter set-render-context-texture!
  render-context-texture)

(define-context-setter set-render-context-shader!
  render-context-shader)

(define-context-setter set-render-context-mesh!
  render-context-mesh)

(define-context-setter set-render-context-framebuffer!
  render-context-framebuffer)

(define-context-setter set-render-context-viewport!
  render-context-viewport)

(define (render-context-transform context)
  (q-front (render-context-transform-stack context)))

(define (render-context-push-transform! context t)
  (q-push! (render-context-transform-stack context) t))

(define render-context-pop-transform!
  (compose q-pop! render-context-transform-stack))

(define (copy-transform! src dest)
  (array-copy! (transform-matrix src) (transform-matrix dest)))

;; emacs: (put 'with-transform-excursion 'scheme-indent-function 1)
(define-syntax-rule (with-transform-excursion context body ...)
  (let ((t (render-context-pop-transform! context)))
    (dynamic-wind
      (lambda ()
        (copy-transform! t (render-context-transform context)))
      (lambda () body ...)
      (lambda ()
        (render-context-push-transform! context t)))))

(define (render-context-transform*! context t)
  (let ((dest (render-context-transform context)))
    (with-transform-excursion context
      (transform*! dest (render-context-transform context) t))))

(define (render-context-transform-identity! context)
  (copy-transform! identity-transform (render-context-transform context)))
