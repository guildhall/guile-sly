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

(use-modules (sly animation)
             (sly game)
             (sly render sprite)
             (sly render tileset)
             (sly vector)
             (sly window))

(load "common.scm")

(define (make-demo-animation)
  "Load a texture, split it into 64x64 tiles, and build an animated
sprite out of it."
  (let* ((tiles (load-tileset "images/princess.png" 64 64))
         (frames (vector (tileset-ref tiles 19)
                         (tileset-ref tiles 20)
                         (tileset-ref tiles 21)
                         (tileset-ref tiles 22)
                         (tileset-ref tiles 23)
                         (tileset-ref tiles 24)
                         (tileset-ref tiles 25)
                         (tileset-ref tiles 26))))
    (make-animation frames 6 #t)))

(define sprite (make-sprite (make-demo-animation)
                            #:position #(320 240)))

(add-hook! draw-hook (lambda (dt alpha) (draw-sprite sprite)))

(with-window (make-window #:title "Animation")
  (start-game-loop))
