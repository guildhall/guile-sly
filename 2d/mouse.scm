;;; guile-2d
;;; Copyright (C) 2013 David Thompson <dthompson2@worcester.edu>
;;;
;;; Guile-2d is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-2d is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Keyboard signals.
;;
;;; Code:

(define-module (2d mouse)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (2d game)
  #:use-module (2d signals)
  #:use-module (2d vector2)
  #:export (mouse-position
            mouse-last-down
            mouse-last-up
            mouse-down?))

(define mouse-last-down (make-signal))
(define mouse-last-up (make-signal))
(define mouse-position (make-signal (vector2 0 0)))

(define (mouse-down? button)
  "Create a signal for the state of BUTTON. Value is #t when mouse
button is pressed and #f otherwise."
  (make-signal
   #:filter (lambda (value old from)
              (eq? value button))
   #:transformer (lambda (value old from)
                   (if (eq? from mouse-last-down) #t #f))
   #:connectors (list mouse-last-down mouse-last-up)))

(register-event-handler
 'mouse-motion
 (lambda (e)
   (signal-set! mouse-position
                (vector2 (SDL:event:motion:x e)
                         (SDL:event:motion:y e)))))

(register-event-handler
 'mouse-down
 (lambda (e)
   (signal-set! mouse-last-down (SDL:event:button:button e))))

(register-event-handler
 'mouse-up
 (lambda (e)
   (signal-set! mouse-last-up (SDL:event:button:button e))))
