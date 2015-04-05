;;; Sly
;;; Copyright (C) 2014 Jordan Russell <jordan.likes.curry@gmail.com>
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

;;; Commentary:
;;
;; Joystick example code
;;
;;; Code:

(use-modules (sly game)
             (sly signal)
             (sly window)
             (sly math vector)
             (sly input joystick)
             (sly render camera)
             (sly render model)
             (sly render sprite)
             (sly render texture)
             (sly render font))

(load "common.scm")

(enable-joystick)
(enable-fonts)

(define font (load-default-font 18))

(define resolution (vector2 640 480))

(define player (load-sprite "images/p1_front.png"))

(define-signal player-position
  (signal-fold v+ (vector2 320 240)
               (signal-map (lambda (v) (v* v 8))
                           (signal-sample 1 (make-directional-signal 0 0 1)))))

(define* (button-caption-signal text button #:optional (joystick 0))
  (let ((false-message (format #f "Released button ~d" button)))
    (signal-map (lambda (x) (if x text false-message))
                (button-down? joystick button))))

(define-signal caption
  (signal-map (lambda (text)
                (model-move (vector2 -76 -90) (label font text)))
              (signal-merge
               (make-signal "Press a button")
               (button-caption-signal "Hello there" 0)
               (button-caption-signal "Thanks for pressing button 1" 1)
               (button-caption-signal "This is the other caption" 2)
               (button-caption-signal "This is the other other caption" 3))))

(define-signal scene
  (signal-map (lambda (position caption)
                (model-move position (model-group player caption)))
              player-position caption))

(define camera (orthographic-camera (vx resolution) (vy resolution)))

(add-hook! draw-hook (lambda _ (draw-model (signal-ref scene) camera)))

(add-hook! joystick-axis-hook
           (lambda (which axis value)
             (format #t "joystick: ~d axis: ~d raw-value: ~d value: ~f~%"
                     which axis value (axis-scale value))))

(add-hook! joystick-button-press-hook
           (lambda (which button)
             (format #t "button-press: (joystick: ~d button: ~d)~%"
                     which button)))

(add-hook! joystick-button-release-hook
           (lambda (which button)
             (format #t "button-release: (joystick: ~d button: ~d)~%"
                     which button)))

(with-window (make-window #:title "Joystick test"
                          #:resolution resolution)
             (start-game-loop))

;;; Local Variables:
;;; compile-command: "../pre-inst-env guile joystick.scm"
;;; End:
