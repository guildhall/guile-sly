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
  #:export (make-model
            model
            model-inherit
            model? model-null?
            null-model
            model-mesh model-transform model-texture model-shader model-color
            model-blend-mode model-depth-test? model-sub-scene model-children
            draw-model
            model-paint
            model-blend
            model-group
            list->model
            model-move
            model-scale
            model-place))

;; Representation of a single OpenGL render call.
(define-record-type <model>
  (%make-model mesh transform texture shader color blend-mode
               depth-test? sub-scene children)
  model?
  (mesh model-mesh)
  (transform model-transform)
  (texture model-texture)
  (shader model-shader)
  (color model-color)
  (blend-mode model-blend-mode)
  (depth-test? model-depth-test?)
  (sub-scene model-sub-scene)
  (children model-children))

(define* (make-model #:optional #:key (mesh null-mesh)
                     (transform identity-transform) (texture null-texture)
                     (shader (load-default-shader)) (color white)
                     (blend-mode default-blend-mode) (depth-test? #t)
                     sub-scene (children '()))
  "Create a new model from MESH and the given rendering state.  When
rendering, TEXTURE and SHADER are bound, BLEND-MODE and DEPTH-TEST?
are set, and the COLOR uniform variable is set.  The presence of a
SUB-SCENE indicates that the model uses the scene's framebuffer as
it's texture, so it must be rendered first."
  (%make-model mesh transform texture shader color blend-mode
               depth-test? sub-scene children))

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

(define null-model
  (make-model #:shader null-shader-program))

(define (model-null? model)
  "Return #t if MODEL has no mesh and no children."
  (and (eq? (model-mesh model) null-mesh)
       (null? (model-children model))))

(define (set-transform-identity! t)
  (let ((matrix (transform-matrix t)))
    (array-set! matrix 1 0 0)
    (array-set! matrix 0 0 1)
    (array-set! matrix 0 0 2)
    (array-set! matrix 0 0 3)
    (array-set! matrix 0 1 0)
    (array-set! matrix 1 1 1)
    (array-set! matrix 0 1 2)
    (array-set! matrix 0 1 3)
    (array-set! matrix 0 2 0)
    (array-set! matrix 0 2 1)
    (array-set! matrix 1 2 2)
    (array-set! matrix 0 2 3)
    (array-set! matrix 0 3 0)
    (array-set! matrix 0 3 1)
    (array-set! matrix 0 3 2)
    (array-set! matrix 1 3 3)))

;; Avoid circular dependency.
(define draw-sub-scene
  (delay (module-ref (resolve-interface '(sly render scene)) 'draw-scene)))

(define (draw-model model view context)
  "Render MODEL by applying its transform (multiplied by VIEW), texture,
shader, vertex array, uniforms, blend mode, etc. to the render
CONTEXT."
  (match model
    ((? model-null? _)
     *unspecified*)
    (($ <model> mesh local-transform texture shader color blend-mode
        depth-test? sub-scene children)

     (when sub-scene
       (with-render-context-excursion context
         ((force draw-sub-scene) sub-scene context)))

     (with-transform-excursion context
       (render-context-transform*! context local-transform)
       (with-transform-excursion context
         (render-context-transform*! context view)
         (set-render-context-depth-test?! context depth-test?)
         (set-render-context-blend-mode! context blend-mode)
         (set-render-context-shader! context shader)
         (set-render-context-mesh! context mesh)
         (set-render-context-texture! context texture)
         ;; TODO: Support user-defined uniforms.
         (uniform-set! shader "mvp" (render-context-transform context))
         (uniform-set! shader "color" color)
         (uniform-set! shader "use_texture" (not (texture-null? texture)))
         (glDrawElements (begin-mode triangles)
                         (mesh-length mesh)
                         (data-type unsigned-int)
                         %null-pointer))
       (for-each (lambda (child)
                   (draw-model child view context))
                 children)))))

;;;
;;; Utility Procedures
;;;

(define (model-paint color model)
  "Create a copy of MODEL, but with a new COLOR."
  (model-inherit model #:color color))

(define (model-blend blend-mode model)
  "Create a copy of MODEL, but with a new BLEND-MODE."
  (model-inherit model #:blend-mode blend-mode))

(define (model-group . children)
  "Create a new compound model containing the list of CHILDREN."
  (make-model #:children children))

(define (list->model children)
  "Create a new compound model containing the list of CHILDREN."
  (make-model #:children children))

(define (model-move position model)
  "Create a new group in which the list of CHILDREN are translated by
the vector POSITION."
  (model-inherit model #:transform (transform* (model-transform model)
                                               (translate position))))

(define (model-scale factor model)
  "Create a version of MODEL that is scaled up/down by FACTOR."
  (model-inherit model #:transform (transform* (model-transform model)
                                               (scale factor))))

(define (model-place transform model)
  "Create a new group in which the tranformation matrices of the
CHILDREN are multiplied by TRANSFORM."
  (model-inherit model #:transform (transform* (model-transform model)
                                               transform)))
