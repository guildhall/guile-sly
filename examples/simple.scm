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

(use-modules (sly game)
             (sly window)
             (sly math vector)
             (sly render camera)
             (sly render group)
             (sly render sprite))

(load "common.scm")

(define scene
  (group-move (vector2 320 240)
              (group (load-sprite "images/p1_front.png"))))

(define camera (orthographic-camera 640 480))

(add-hook! draw-hook (lambda _ (draw-group scene camera)))

(with-window (make-window #:title "Simple Sprite Demo")
  (start-game-loop))

;;; Local Variables:
;;; compile-command: "../pre-inst-env guile simple.scm"
;;; End:
