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
             (sly window)
             (sly signal)
             (sly utils)
             (sly render camera)
             (sly render color)
             (sly render group)
             (sly render model)
             (sly render sprite)
             (sly render texture)
             (sly render tileset)
             (sly math vector)
             (sly math tween)
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
  #2((208 209 209 209 209 209 209 210)
     (224 225 225 225 225 225 225 226)
     (224 225 225 225 225 225 225 226)
     (224 225 225 176 177 225 225 226)
     (224 225 225 192 193 225 225 226)
     (224 225 225 225 225 225 225 226)
     (224 225 225 225 225 225 225 226)
     (240 241 241 241 241 241 241 242)))

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
         (sprite texture #:anchor 'bottom-left)))))

  (define (build-tile x y)
    (group-move (vector2 (* x (tileset-width tileset))
                         (* y (tileset-height tileset)))
                (group (build-sprite (array-ref tiles y x)))))

  (match (array-dimensions tiles)
    ((height width)
     (make-group (list-ec (: x width) (: y height)
                          (build-tile x y))))))

(define tileset (load-tileset "images/tiles.png" 32 32))

(define scene
  (group-move (v- (vector2 320 240)
                  (v* (vector2 tile-width tile-height) 4))
              (group (build-map-layer map-tiles tileset))))

(define camera
  (orthographic-camera 640 480))

(add-hook! draw-hook (lambda _ (draw-group scene camera)))

(with-window (make-window #:title "Tilemap")
  (start-game-loop))

;;; Local Variables:
;;; compile-command: "../pre-inst-env guile tilemap.scm"
;;; End:
