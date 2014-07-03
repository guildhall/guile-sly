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

(use-modules (sly game)
             (sly sprite)
             (sly vector)
             (sly window)
             (sly color)
             (sly signal)
             (sly texture)
             (sly transition))

(load "common.scm")

(define texture (load-texture "images/p1_front.png"))

(define sprite
  (signal-map
   (lambda (position color)
     (make-sprite texture
                  #:position position
                  #:color color))
   (transition #(32 240) #(608 240) 120
               #:ease ease-in-out-quad)
   (transition white tango-plum 120)))

(add-hook! draw-hook (lambda (dt alpha)
                       (draw-sprite (signal-ref-maybe sprite))))

(with-window (make-window #:title "Transitions")
  (start-game-loop))
