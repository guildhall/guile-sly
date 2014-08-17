;;; Sly
;;; Copyright (C) 2014 David Thompson <davet@gnu.org>
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
             (srfi srfi-26)
             (sly agenda)
             (sly camera)
             (sly coroutine)
             (sly game)
             (sly keyboard)
             (sly vector)
             (sly window)
             (sly shader)
             (sly signal)
             (sly color)
             (sly texture)
             (sly transform)
             (sly math)
             (sly font)
             (sly rect)
             (sly transition)
             (sly mesh)
             (sly scene)
             (sly shape)
             (sly sprite)
             (sly wrappers gl))

(load "common.scm")
(enable-fonts)

(define tex (load-texture "images/crate.png"))

(define unit-cube (make-cube 1 #:texture tex))

(define my-scene
  (scene-root
   (scene-node #:mesh unit-cube
               #:position #(0 0 0)
               #:scale 2)
   (signal-map
    (lambda (down?)
      (scene-node #:mesh unit-cube
                  #:position #(0 2 0)
                  #:scale (if down? (transition 2 3/2 15) 3/2)))
    (key-down? 'space))
   (scene-node #:mesh unit-cube
               #:position #(0 4 0))
   (scene-node #:mesh unit-cube
               #:position #(0 6 0)
               #:scale 1/2)
   (scene-node #:mesh unit-cube
               #:position #(0 0 0))
   (scene-node #:mesh unit-cube
               #:position #(3 0 0)
               #:scale 2)
   (scene-node #:mesh unit-cube
               #:position #(-3 0 0)
               #:scale 2
               #:children (list
                           (scene-node #:mesh unit-cube
                                       #:position (transition #(0 2 0)
                                                              #(0 1 0)
                                                              120))))))

(define gui
  (scene-root
   (scene-node #:mesh (make-sprite (load-texture "images/p1_front.png"))
               #:position #(100 100))
   (scene-node #:mesh (make-label (load-default-font 16)
                                  "Testing the scene graph"
                                  #:color red)
               #:translate #(10 10)
               #:rotation -1/8)))

(define camera
  (make-camera my-scene
               (look-at #(0 0 7) #(0 0 0))
               (perspective-projection 60 4/3 1/10 20)
               (make-rect 0 0 320 240)
               #:clear-color tango-aluminium-3))

(define camera2
  (make-camera my-scene
               (look-at #(2 3 5) #(0 0 0))
               (perspective-projection 60 4/3 1/10 20)
               (make-rect 320 0 320 240)
               #:clear-color tango-aluminium-4))

(define camera3
  (make-camera my-scene
               (signal-map (lambda (angle)
                             (transform*
                              (look-at #(0 0 8) #(0 0 0))
                              (rotate-y angle)))
                           (transition 0 (* 2 pi) (* 60 3)))
               (perspective-projection 60 4/3 1/10 20)
               (make-rect 320 240 320 240)
               #:clear-color tango-aluminium-5))

(define camera4
  (make-camera gui
               identity-transform
               (orthographic-projection 0 640 0 480 0 1)
               (make-rect 0 0 640 480)
               #:clear-color tango-aluminium-6
               #:clear-flags '(depth-buffer)))

(with-window (make-window #:title "Scene Graph")
  (start-game-loop (list camera camera2 camera3 camera4)))

;; Local Variables:
;; coding: utf-8
;; compile-command: "../pre-inst-env guile scene-graph.scm"
;; End:
