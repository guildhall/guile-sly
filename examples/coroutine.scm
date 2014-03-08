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

(use-modules (2d agenda)
             (2d coroutine)
             (2d game)
             (2d sprite)
             (2d vector2)
             (2d window))

(load "common.scm")

(define window-width 640)
(define window-height 480)

(define sprite (load-sprite "images/p1_front.png"
                            #:position (vector2 320 240)))

;; Simple script that moves the sprite to a random location every
;; second.
(coroutine
 (while #t
   (set-sprite-position!
    sprite
    (vector2 (random window-width)
             (random window-height)))
   (wait game-agenda 15)
   (set-sprite-rotation! sprite (random 360))
   (wait game-agenda 15)))

(add-hook! draw-hook (lambda (dt alpha) (draw-sprite sprite)))

(with-window (make-window #:title "Coroutines"
                          #:resolution (vector2 window-width
                                                window-height))
  (run-game-loop))
