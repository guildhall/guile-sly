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

(use-modules (srfi srfi-26)
             (sly agenda)
             (sly coroutine)
             (sly game)
             (sly sprite)
             (sly texture)
             (sly vector)
             (sly window))

(load "common.scm")

(define window-width 640)
(define window-height 480)

(define texture (load-texture "images/p1_front.png"))

(define sprite
  (make-sprite texture
               #:position #(320 240)))

(define (random-vector2)
  (vector (random window-width)
          (random window-height)))

;; Simple script that moves the sprite to a random location every
;; second.
(coroutine
 (while #t
   (set! sprite (set-sprite-position sprite (random-vector2)))
   (wait 15)
   (set! sprite (set-sprite-rotation sprite (random 360)))
   (wait 15)))

(add-hook! draw-hook (lambda (dt alpha) (draw-sprite sprite)))

(with-window (make-window #:title "Coroutines"
                          #:resolution (vector window-width
                                               window-height))
  (start-game-loop))
