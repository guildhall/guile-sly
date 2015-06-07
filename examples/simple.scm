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
             (sly utils)
             (sly signal)
             (sly math transform)
             (sly math vector)
             (sly render camera)
             (sly render model)
             (sly render sprite)
             (sly render texture)
             (sly render color)
             (sly render framebuffer)
             (sly render scene)
             (sly render context))

(load "common.scm")

(define s (load-sprite "images/p1_front.png"))

(define bg (load-sprite "images/lava.png"))

(define fb (make-framebuffer 320 240))

(define base-model
  (model-move (vector2 160 120) s))

(define base-camera
  (orthographic-camera 320 240))

(define base-scene
  (scene base-camera base-model fb))

(define model
  (model-group (model-move (vector2 320 240) bg)
               (chain (scene->sprite base-scene)
                 (model-move (vector2 320 240))
                 (model-scale 2))))

(define camera (orthographic-camera 640 480))

(define-signal scene (make-scene camera model))

(with-window (make-window #:title "Simple Sprite Demo")
  (start-game-loop scene))

;;; Local Variables:
;;; compile-command: "../pre-inst-env guile simple.scm"
;;; End:
