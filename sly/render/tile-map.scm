;;; Sly
;;; Copyright (C) 2015 David Thompson <davet@gnu.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Convenience procedures for 2D tile maps.
;;
;;; Code:

(define-module (sly render tile-map)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (sly utils)
  #:use-module (sly math vector)
  #:use-module (sly render mesh)
  #:use-module (sly render model)
  #:use-module (sly render shader)
  #:use-module (sly render texture)
  #:use-module (sly render tileset)
  #:export (compile-tile-layer))

(define* (compile-tile-layer tiles tile-width tile-height
                             #:key (shader (load-default-shader)))
  "Compile the two-dimensional vlist TILES into a list of models for
efficient rendering.  The resulting map spaces each tile by TILE-WIDTH
and TILE-HEIGHT.  The compiled models all use the given SHADER when
rendered.  TILES is assumed to be rectangular, with each row having
equal elements."
  (define (make-tile-vertices x y tile)
    (let* ((x1 (* x tile-width))
           (y1 (* y tile-height))
           (x2 (+ x1 (texture-width tile)))
           (y2 (+ y1 (texture-height tile)))
           (s1 (texture-s1 tile))
           (t1 (texture-t1 tile))
           (s2 (texture-s2 tile))
           (t2 (texture-t2 tile)))
      (list '(0 3 2 0 2 1)
            (list
             (vector3 x1 y1 0)
             (vector3 x2 y1 0)
             (vector3 x2 y2 0)
             (vector3 x1 y2 0))
            (list
             (vector2 s1 t1)
             (vector2 s2 t1)
             (vector2 s2 t2)
             (vector2 s1 t2)))))

  (define append-vertices
    (match-lambda*
     (((index-a pos-a tex-a) (index-b pos-b tex-b))
      (list (append index-a index-b)
            (append pos-a pos-b)
            (append tex-a tex-b)))))

  (define (add-tile-vertices meshes x y)
    (let* ((tile (vlist-ref* tiles y x))
           (vertices (make-tile-vertices x y tile)))

      (define same-texture?
        (let ((parent-texture (texture-parent tile)))
          (lambda (texture)
            (eq? texture parent-texture))))

      (let loop ((meshes meshes))
        (match meshes
          (()
           (list (list (texture-parent tile) vertices)))
          ((((? same-texture? texture) existing-vertices) . rest)
           (let ((new-vertices (append-vertices vertices existing-vertices)))
             (cons (list texture new-vertices) rest)))
          ((head tail ...)
           (cons head (loop tail)))))))

  (define vertices
    (let ((height (vlist-length tiles))
          (width (vlist-length (vlist-ref tiles 0))))
      (let loop ((x 0)
                 (y 0)
                 (meshes '()))
        (cond
         ((>= y height)
          meshes)
         ((>= x width)
          (loop 0 (1+ y) meshes))
         (else
          (let ((meshes (add-tile-vertices meshes x y)))
            (loop (1+ x) y meshes)))))))

  (define (offset-indices indices)
    (let loop ((indices indices)
               (result '())
               (x 0))
      (match indices
        (() (reverse result))
        ((a b c d e f . tail)
         (let* ((offset (* x 4))
                (new (cons* (+ f offset)
                            (+ e offset)
                            (+ d offset)
                            (+ c offset)
                            (+ b offset)
                            (+ a offset)
                            result)))
           (loop tail new (1+ x)))))))

  (map (match-lambda
        ((texture (indices positions textures))
         (make-model #:mesh (build-mesh (list->vector (offset-indices indices))
                                        (list->vector positions)
                                        (list->vector textures))
                     #:texture texture
                     #:shader shader)))
       vertices))
