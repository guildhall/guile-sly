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

(use-modules
 (sly game)
 (sly repl)
 (sly sprite)
 (sly render texture)
 (sly joystick)
 (sly signal)
 (sly window)
 (sly vector)
 (sly font))

(open-window)
(start-sly-repl)
(enable-sprites)
(enable-joystick)
(enable-fonts)

(define default-font (load-default-font))

(define resolution (vector 640 480))

(add-hook! window-close-hook stop-game-loop)

(define p1-texture (load-texture "images/p1_front.png"))

(define-signal p1-position
  (signal-fold v+ (vector 320 240)
               (signal-map
                (lambda (v)
                  (v* (vector 8 8) v))
                (signal-sample 1
                               (make-directional-signal 0 0 1)))))

(define* (button-caption-signal text button #:optional (joystick 0))
  (signal-map (lambda (x)
                (if x text ""))
              (button-down? joystick button)))

(define-signal p1-caption
  (signal-map (lambda (text pos)
                (make-label default-font
                            text
                            (v+ (vector -76 -90) pos)))
              (signal-merge
               (button-caption-signal "Hello there" 0)
               (button-caption-signal "Thanks for pressing button 1" 1)
               (button-caption-signal "This is the other caption" 2)
               (button-caption-signal "This is the other other caption" 3))
              p1-position))

(define (draw-p1-caption dt alpha)
  (draw-label (signal-ref p1-caption)))

(define (draw-p1 dt alpha)
  (draw-sprite (make-sprite p1-texture
                            #:position (signal-ref p1-position))))

(add-hook! draw-hook draw-p1)

(add-hook! draw-hook draw-p1-caption)

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
