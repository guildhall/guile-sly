;;; Sly
;;; Copyright (C) 2014 David Thompson <dthompson2@worcester.edu>
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
;; Scene graph API.
;;
;;; Code:

(define-module (sly scene-graph)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (gl)
  #:use-module (sly wrappers gl)
  #:use-module (sly color)
  #:use-module (sly rect)
  #:use-module (sly signal)
  #:use-module (sly transform)
  #:use-module (sly vector)
  #:export (make-scene-node
            scene-node?
            scene-node-transform
            scene-node-color
            scene-node-texture
            scene-node-shader
            scene-node-arrays
            scene-node-children
            draw-scene-node))

;; Types of scene nodes:
;; - Group: position, scale, rotation, children
;; - Vertices: vertex arrays + style (shader, texture)

;; All nodes have the following
;; - transform
;; - children
;; - shader
;; - texture
;; - vertex array

;; (define (sprite)
;;   (make-scene-node #:transform (make-transform* #:translate (vector2 100 100))
;;                    #:color white
;;                    #:texture (load-texture "images/p1_front.png")
;;                    #:arrays `((vertex float ,vertices
;;                                       #:stride ,texture-vertex-size
;;                                       #:offset ,x-offset)
;;                               (texture float ,vertices
;;                                        #:stride ,texture-vertex-size
;;                                        #:offset ,s-offset))
;;                    #:shader sprite-shader))

;;;
;;; Meshes
;;;

(define-vertex sprite-vertex
  (position vector2)
  (texcoord vector2))



(define-record-type <geometry>
  (make-geometry primitive size arrays)
  geometry?
  (primitive geometry-primitive)
  (size geometry-size)
  (arrays geometry-arrays))

(define gl-primitives
  (alist->hash-table
   '((points . 0)
     (lines . 1)
     (line-loop . 2)
     (line-strip . 3)
     (triangles . 4)
     (triangle-strip . 5)
     (triangle-fan . 6)
     (quads . 7)
     (quad-strip . 8)
     (polygon . 9))))

(define gl-arrays
  (alist->hash-table
   '((vertex-array . 32884)
     (normal-array . 32885)
     (color-array . 32886)
     (index-array . 32887)
     (texture-coord-array . 32888)
     (edge-flag-array . 32889))))

(define gl-types
  (alist->hash-table
   '((byte . 5120)
     (unsigned-byte . 5121)
     (short . 5122)
     (unsigned-short . 5123)
     (int . 5124)
     (unsigned-int . 5125)
     (float . 5126)
     (#{2-bytes}# . 5127)
     (#{3-bytes}# . 5128)
     (#{4-bytes}# . 5129)
     (double . 5130)
     (double-ext . 5130))))

(define (draw-geometry geometry)
  (define (iter arrays)
    (cond
     ((null? arrays)
      (let ((primitive (hash-ref gl-primitives (geometry-primitive geometry))))
        (gl-draw-arrays primitive 0 (geometry-size geometry))))
     (else
      (match (car arrays)
        ((array-type data-type vertices . rest)
         (with-gl-client-state (hash-ref gl-arrays array-type)
           (set-gl-vertex-array (hash-ref gl-types data-type) vertices)
           (iter (cdr arrays))))))))
  (iter (geometry-arrays geometry)))

(define foo
  '(vertex float #(1 2 3 4) #:stride 4 #:offset 8))

(match foo
  ((array-type data-type vertices . kwargs)
   (format #t "~a ~a ~a\n" array-type data-type vertices)
   (let lp ((kwargs kwargs))
     (match kwargs
       ((key value . rest)
        (format #t "~a - ~a\n" key value)
        (lp rest))
       (_ #f)))))

(define-record-type <scene-node>
  (%make-scene-node transform color texture shader arrays children)
  scene-node?
  (transform scene-node-transform)
  (color scene-node-color)
  (texture scene-node-texture)
  (shader scene-node-shader)
  (arrays scene-node-arrays)
  (primitive scene-node-primitive)
  (children scene-node-children))

(define* (make-scene-node #:optional #:key (transform identity-transform)
                          (color white) (texture #f) (shader #f)
                          (arrays '()) (children '()))
  (%make-scene-node transform color texture shader arrays children))

(define* (draw-scene-node node #:optional (transform identity-transform))
  (let ((transform (transform* transform (scene-node-transform node))))
    ;; WRITEME
    (for-each (cut draw-scene-node <> transform)
              (scene-node-children node))))

(define (draw-arrays node)
  #f)

;; (define (draw-sprite transform)
;;   (display "drew a sprite\n"))

;; (define (draw-label transform)
;;   (display "drew a label\n"))

;; (define s
;;   (make-scene-node
;;    #:position (vector2 100 100)
;;    #:size (vector2 200 200)
;;    #:proc draw-sprite
;;    #:children
;;    (list (make-scene-node
;;           #:position (vector2 50 50)
;;           #:size (vector2 50 50)
;;           #:rotation 45
;;           #:proc draw-label))))

;; (draw-scene-node s)

;; (define scene
;;   (scene-graph
;;    (sprite
;;     (position (vector2 320 240))
;;     (texture "logo.png")
;;     (anchor 'center))
;;    (label
;;     (position (vector2 320 160))
;;     (text "Some Really Awesome Game")
;;     (anchor 'bottom-center))
;;    (group
;;     (position (vector2 160 300))
;;     (children
;;      (sprite
;;       (position (vector2 16 16))
;;       (texture "load-game.png"))
;;      (sprite
;;       (position (vector2 64 16))
;;       (texture "new-game.png"))
;;      (sprite
;;       (position (vector2 96 16))
;;       (texture "options.png"))))))
