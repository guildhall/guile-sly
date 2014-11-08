;;; Sly
;;; Copyright (C) 2013, 2014 David Thompson <dthompson2@worcester.edu>
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

(use-modules (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-42)
             (sly game)
             (sly render sprite)
             (sly render texture)
             (sly render tileset)
             (sly vector)
             (sly window)
             (sly render scene)
             (sly signal)
             (sly camera)
             (sly render color)
             (sly transition)
             (sly utils)
             (sly input keyboard))

(load "common.scm")

;;;
;;; Orthogonal tile map example
;;;

;; This is a quick and dirty tile map implementation. No fancy map
;; loading. Just a hardcoded tile map that demonstrates the
;; split-texture procedure.

;; A small 8x8 array of tile indices.
(define map-width 8)
(define map-height 8)
(define map-tiles
  #2((00 01 01 01 01 01 01 02)
     (16 17 17 17 17 17 17 18)
     (16 17 17 17 17 17 17 18)
     (16 17 17 48 49 17 17 18)
     (16 17 17 64 65 17 17 18)
     (16 17 17 17 17 17 17 18)
     (16 17 17 17 17 17 17 18)
     (32 33 33 33 33 33 33 34)))

(define tile-width 32)
(define tile-height 32)

(define (random-map width height tileset)
  (let ((tiles (make-array 0 height width))
        (n (vector-length (tileset-tiles tileset))))
    (do-ec (: y height) (: x width)
           (array-set! tiles (random n) y x))
    tiles))

(define (build-map-layer tiles tileset)
  (define build-sprite
    (memoize
     (lambda (tile-index)
       (let ((texture (tileset-ref tileset tile-index)))
         (make-sprite texture #:anchor #(0 0))))))

  (define (build-tile x y)
    (scene-node
     (position (vector (* x (tileset-width tileset))
                       (* y (tileset-height tileset))))
     (uniforms `(("color" ,white)))
     (children (list (build-sprite (array-ref tiles y x))))))

  (match (array-dimensions tiles)
    ((height width)
     (scene-node
      (children
       (list-ec (: x width) (: y height)
                (build-tile x y)))))))

(define tileset (load-tileset "images/tiles.png" 32 32))

(define-signal map-scene
  (scene-root
   (scene-node
    (position
     (vector (- 320 (* tile-width 4))
             (- 240 (* tile-height 4))))
    (children
     (list (build-map-layer map-tiles tileset))))))

(define-signal camera
  (orthographic-camera map-scene 640 480))

(with-window (make-window #:title "Tilemap")
  (start-game-loop camera))

;;; Local Variables:
;;; compile-command: "../pre-inst-env guile tilemap.scm"
;;; End:
