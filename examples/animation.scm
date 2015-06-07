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
             (sly signal)
             (sly math)
             (sly math tween)
             (sly math vector)
             (sly render camera)
             (sly render model)
             (sly render sprite)
             (sly render tileset)
             (sly render scene))

(load "common.scm")

(define walk-cycle
  (let ((tiles (load-tileset "images/princess.png" 64 64)))
    (list->vector
     (map (lambda (id)
            (sprite (tileset-ref tiles id)))
          '(19 20 21 22 23 24 25 26)))))

(define position-tween
  (tween vlerp (compose ease-linear ease-loop)
         (vector2 480 240) (vector2 160 240) 120))

(define frame-tween
  (let* ((frame-count (vector-length walk-cycle))
         (frame-rate (/ 60 frame-count)))
    (tween (compose floor lerp) (compose ease-linear ease-loop)
           0 frame-count (* frame-count frame-rate))))

(define camera (orthographic-camera 640 480))

(define-signal scene
  (signal-let ((time (signal-timer)))
    (let* ((frame (vector-ref walk-cycle (frame-tween time)))
           (model (model-move (position-tween time) frame)))
      (make-scene camera model))))

(with-window (make-window #:title "Animation")
  (start-game-loop scene))

;;; Local Variables:
;;; compile-command: "../pre-inst-env guile animation.scm"
;;; End:
