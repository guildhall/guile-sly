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
             (sly helpers)
             (sly tileset)
             (sly wrappers gl))

(load "common.scm")
(enable-fonts)

(define tex (load-texture "images/crate.png"))

(define unit-cube (make-cube 1 #:texture tex))

(define my-scene
  (scene-root
   (scene-node #:position #(0 0 0)
               #:scale (signal-generator
                        (forever
                          (yield 1/2)
                          (wait 30)
                          (yield 1)
                          (wait 30)
                          (yield 2)
                          (wait 30)))
               #:children (list unit-cube))
   (signal-map
    (lambda (down?)
      (scene-node #:position #(0 2 0)
                  #:scale (if down? (transition 2 3/2 15) 3/2)
                  #:children (list unit-cube)))
    (key-down? 'space))
   (scene-node  #:position #(0 4 0)
                #:children (list unit-cube))
   (scene-node #:position #(0 6 0)
               #:scale 1/2
               #:children (list unit-cube))
   (scene-node #:position #(3 0 0)
               #:scale 2
               #:children (list unit-cube))
   (scene-node #:position #(-3 0 0)
               #:scale 2
               #:children (list
                           unit-cube
                           (scene-node #:position (transition #(0 2 0)
                                                              #(0 1 0)
                                                              120)
                                       #:children (list unit-cube))))))

(define animation
  (let ((tiles (load-tileset "images/princess.png" 64 64)))
    (list (tileset-ref tiles 19)
          (tileset-ref tiles 20)
          (tileset-ref tiles 21)
          (tileset-ref tiles 22)
          (tileset-ref tiles 23)
          (tileset-ref tiles 24)
          (tileset-ref tiles 25)
          (tileset-ref tiles 26))))

(define font (load-default-font 16))

(define gui
  (scene-root
   (scene-node  #:position #(100 100)
                #:children (list (make-animated-sprite animation 6)))
   (scene-node #:translate #(10 10)
               #:rotation -1/8
               #:children (list
                           (signal-generator
                            (let ((message "Testing the scene graph"))
                              (forever
                               (let loop ((i 1))
                                 (when (<= i (string-length message))
                                   (yield (make-label font
                                                      (substring message 0 i)
                                                      #:color red))
                                   (wait 3)
                                   (loop (1+ i))))
                               (wait 60))))))))

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
