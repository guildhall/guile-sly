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

(use-modules (sly camera)
             (sly game)
             (sly math rect)
             (sly render scene)
             (sly render sprite)
             (sly math transform)
             (sly vector)
             (sly window)
             (sly color))

(load "common.scm")

(define scene
  (scene-root
   (scene-node
    (position #(320 240))
    (uniforms `(("color" ,white)))
    (children
     (list (load-sprite "images/p1_front.png"))))))

(define camera
  (make-camera scene
               identity-transform
               (orthographic-projection 0 640 0 480 0 1)
               (make-rect 0 0 640 480)))

(with-window (make-window #:title "Simple Sprite Demo")
  (start-game-loop camera))

;;; Local Variables:
;;; compile-command: "../pre-inst-env guile simple.scm"
;;; End:
