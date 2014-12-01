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
;; OpenGL rendering state.
;;
;;; Code:

(define-module (sly render model)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (gl)
  #:use-module (gl low-level)
  #:use-module (sly math transform)
  #:use-module (sly math vector)
  #:use-module (sly math rect)
  #:use-module (sly render shader)
  #:use-module (sly render texture)
  #:use-module (sly render utils)
  #:use-module (sly render camera)
  #:use-module (sly render color)
  #:use-module (sly render context)
  #:use-module (sly render mesh)
  #:export (make-model model model-inherit
            model?
            model-mesh model-texture model-shader model-color
            model-blend-mode model-depth-test?
            draw-model
            model-paint model-blend))

;; Representation of a single OpenGL render call.
(define-record-type <model>
  (%make-model mesh texture shader color blend-mode depth-test?)
  model?
  (mesh model-mesh)
  (texture model-texture)
  (shader model-shader)
  (color model-color)
  (blend-mode model-blend-mode)
  (depth-test? model-depth-test?))

(define* (make-model #:optional #:key (mesh #f) (texture #f) (shader #f)
                     (color white) (blend-mode default-blend-mode)
                     (depth-test? #t))
  "Create a new model from MESH and the given rendering state.  When
rendering, TEXTURE and SHADER are bound, BLEND-MODE and DEPTH-TEST?
are set, and the COLOR uniform variable is set."
  (%make-model mesh texture shader color blend-mode depth-test?))

(define model make-model)

(define kwargs->alist
  (match-lambda
   (((? keyword? key) value . rest)
    (cons (cons (keyword->symbol key) value) (kwargs->alist rest)))
   (() '())))

(define model-inherit
  (let* ((fields (record-type-fields <model>))
         (field-indices (iota (length fields))))
    (lambda (original . kwargs)
      "Create a new model based on the fields of ORIGINAL, only
changing the fields specified in KWARGS."
      (let ((field+value (kwargs->alist kwargs)))
        (apply %make-model
               (map (lambda (field index)
                      (let ((override (find (match-lambda
                                             ((k . v)
                                              (eq? field k)))
                                            field+value)))
                        (if override
                            (cdr override)
                            (struct-ref original index))))
                    fields field-indices))))))

(define (draw-model model world-transform view context)
  "Render MODEL by applying its transform (multiplied by VIEW), texture,
shader, vertex array, uniforms, blend mode, etc. to the render
CONTEXT."
  (match model
    (($ <model> mesh texture shader color blend-mode depth-test?)
     (with-temp-transform context mvp
       (transform*! mvp world-transform view)
       (set-render-context-depth-test?! context depth-test?)
       (set-render-context-blend-mode! context blend-mode)
       (set-render-context-shader! context shader)
       (set-render-context-mesh! context mesh)
       (set-render-context-texture! context texture)
       ;; TODO: Support user-defined uniforms.
       (uniform-set! shader "mvp" mvp)
       (uniform-set! shader "color" color)
       (glDrawElements (begin-mode triangles)
                       (mesh-length mesh)
                       (data-type unsigned-int)
                       %null-pointer)))))

;;;
;;; Utility Procedures
;;;

(define (model-paint color model)
  "Create a copy of MODEL, but with a new COLOR."
  (model-inherit model #:color color))

(define (model-blend blend-mode model)
  "Create a copy of MODEL, but with a new BLEND-MODE."
  (model-inherit model #:blend-mode blend-mode))
