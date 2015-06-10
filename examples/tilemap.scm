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
             (ice-9 vlist)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-26)
             (sly game)
             (sly window)
             (sly signal)
             (sly utils)
             (sly render camera)
             (sly render color)
             (sly render model)
             (sly render mesh)
             (sly render shader)
             (sly render sprite)
             (sly render texture)
             (sly render tileset)
             (sly render tile-map)
             (sly render scene)
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

(define (build-map tile-indices)
  (list->vlist*
   (map (lambda (row)
          (map (cut tileset-ref tileset <>) row))
        tile-indices)))

(define (random-map width height)
  (let ((n (vector-length (tileset-tiles tileset))))
    (list->vlist*
     (list-tabulate
      height
      (lambda (y)
        (list-tabulate
         width
         (lambda (x)
           (tileset-ref tileset (random n)))))))))

(define tile-width 32)
(define tile-height 32)
(define tileset
  (load-tileset "images/tiles.png" tile-width tile-height))

(define map-width 20)
(define map-height 15)
(define map-tiles
  (build-map
   '((65 65 65 65 65 65 65 65 65 224 194 225 194 192 209 210 65 65 65 65)
     (65 65 65 65 65 65 65 65 208 193 225 194 176 241 177 192 210 65 65 65)
     (65 65 65 65 65 65 65 65 224 225 194 194 226 65 240 177 192 210 65 65)
     (65 65 65 65 65 65 65 65 224 225 225 176 242 65 65 240 177 226 65 65)
     (65 65 65 65 65 65 65 208 193 225 225 226 65 65 65 65 224 226 65 65)
     (65 65 65 65 65 65 208 193 194 225 225 226 65 65 65 208 193 226 65 65)
     (65 65 65 65 208 209 193 194 194 225 194 192 209 209 209 193 176 242 65 65)
     (65 65 65 65 224 194 194 194 225 176 241 241 177 225 225 194 192 209 209 209)
     (65 65 65 65 240 177 225 225 176 242 65 65 240 241 241 177 225 225 194 225)
     (65 65 65 208 209 193 225 176 242 65 65 65 65 65 65 240 241 241 241 241)
     (65 65 208 193 225 225 176 242 65 65 65 65 65 65 65 65 65 65 65 65)
     (65 208 193 225 176 241 242 65 65 65 65 65 65 65 65 65 65 65 65 65)
     (208 193 225 176 242 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65)
     (193 225 225 226 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65)
     (225 225 176 242 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65 65))))

(define model
  (model-move (v- (vector2 320 240)
                  (v* (vector2 tile-width tile-height)
                      (vector2 10 15/2)))
              (list->model (compile-tile-layer map-tiles 32 32))))

(define camera
  (orthographic-camera 640 480))

(define-signal scene (make-scene camera model))

(with-window (make-window #:title "Tilemap")
  (run-game-loop scene))

;;; Local Variables:
;;; compile-command: "../pre-inst-env guile tilemap.scm"
;;; End:
