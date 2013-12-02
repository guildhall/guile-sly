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

(define-module (2d keyboard)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (2d game)
  #:use-module (2d signals)
  #:use-module (2d vector2)
  #:export (key-last-down
            key-last-up
            key-down?
            key-directions
            key-arrows
            key-wasd))

(define key-last-down (make-signal))
(define key-last-up (make-signal))

(define (key-down? key)
  "Create a signal for the state of KEY. Value is #t when key is
pressed and #f otherwise."
  (make-signal
   #:filter (lambda (value old from)
              (eq? value key))
   #:transformer (lambda (value old from)
                   (if (eq? from key-last-down) #t #f))
   #:connectors (list key-last-down key-last-up)))

(define (key-directions up down left right)
  (signal-lift4 (lambda (up? down? left? right?)
                  (vector2 (+ (if left? -1 0)
                              (if right? 1 0))
                           (+ (if up? -1 0)
                              (if down? 1 0))))
                (key-down? up)
                (key-down? down)
                (key-down? left)
                (key-down? right)))

(define key-arrows (key-directions 'up 'down 'left 'right))
(define key-wasd (key-directions 'w 's 'a 'd))

(register-event-handler
 'key-down
 (lambda (e)
   (signal-set! key-last-down (SDL:event:key:keysym:sym e))))

(register-event-handler
 'key-up
 (lambda (e)
   (signal-set! key-last-up (SDL:event:key:keysym:sym e))))
