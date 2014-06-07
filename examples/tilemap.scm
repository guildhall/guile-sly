;;; guile-2d
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

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-42)
             (2d game)
             (2d sprite)
             (2d texture)
             (2d tileset)
             (2d vector)
             (2d window))

(load "common.scm")

;;;
;;; Orthogonal tile map example
;;;

;; This is a quick and dirty tile map implementation. No fancy map
;; loading. Just a hardcoded tile map that demonstrates the
;; split-texture procedure.

;; tiles is a 2d array of texture regions.
(define-record-type <map-layer>
  (make-map-layer width height tile-width tile-height tiles)
  map-layer?
  (width map-layer-width)
  (height map-layer-height)
  (tile-width map-layer-tile-width)
  (tile-height map-layer-tile-height)
  (tiles map-layer-tiles))

(define draw-map-layer
  (lambda (layer)
    (do-ec (: y (map-layer-height layer))
           (: x (map-layer-width layer))
           (let ((tile (array-ref (map-layer-tiles layer) y x)))
             (draw-sprite tile)))))

;; A small 8x8 array of tile indices.
(define map-width 8)
(define map-height 8)
(define map-tiles
  #2u32((00 01 01 01 01 01 01 02)
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
        (n (vector-length tileset)))
    (do-ec (: y height) (: x width)
           (array-set! tiles (random n) y x))
    tiles))

(define (tiles->sprites width height tile-width tile-height tileset tiles)
  (define (build-sprite x y)
    (let ((region (tileset-ref tileset (array-ref tiles y x))))
      (make-sprite region
                   #:position (vector (* x tile-width)
                                      (* y tile-height))
                   #:anchor #(0 0))))

  (let ((sprites (list-ec (: y height)
                          (list-ec (: x width)
                                   (build-sprite x y)))))
    (list->array 2 sprites)))

(define (build-map)
  ;; Load tileset and build map layer
  (let ((tileset (load-tileset "images/tiles.png" 32 32)))
    (make-map-layer map-width map-height tile-width tile-height
                    (tiles->sprites map-width
                                    map-height
                                    tile-width
                                    tile-height
                                    tileset
                                    map-tiles))))

(define layer (build-map))

(add-hook! draw-hook (lambda (dt alpha) (draw-map-layer layer)))

(with-window (make-window #:title "Tilemap")
  (start-game-loop))
