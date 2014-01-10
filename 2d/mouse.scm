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
  #:export (mouse-move-hook
            mouse-press-hook
            mouse-click-hook
            mouse-x
            mouse-y
            mouse-position
            mouse-last-down
            mouse-last-up
            mouse-down?))

(define mouse-move-hook (make-hook 2))
(define mouse-press-hook (make-hook 3))
(define mouse-click-hook (make-hook 3))
(define mouse-last-down (make-root-signal 'none))
(define mouse-last-up (make-root-signal 'none))
(define mouse-x (make-root-signal 0))
(define mouse-y (make-root-signal 0))
(define mouse-position (signal-map vector2 mouse-x mouse-y))

(define (mouse-down? button)
  "Create a signal for the state of BUTTON. Value is #t when mouse
button is pressed or #f otherwise."
  (define (same-button? other-button)
    (eq? button other-button))

  (define (button-filter value signal)
    (signal-constant value (signal-filter #f same-button? signal)))

  (signal-merge (button-filter #f mouse-last-up)
                (button-filter #t mouse-last-down)))

(register-event-handler
 'mouse-motion
 (lambda (e)
   (run-hook mouse-move-hook
             (SDL:event:motion:x e)
             (SDL:event:motion:y e))
   (signal-set! mouse-x (SDL:event:motion:x e))
   (signal-set! mouse-y (SDL:event:motion:y e))))

(register-event-handler
 'mouse-button-down
 (lambda (e)
   (run-hook mouse-press-hook
             (SDL:event:button:button e)
             (SDL:event:button:x e)
             (SDL:event:button:y e))
   (signal-set! mouse-last-down (SDL:event:button:button e))))

(register-event-handler
 'mouse-button-up
 (lambda (e)
   (run-hook mouse-click-hook
             (SDL:event:button:button e)
             (SDL:event:button:x e)
             (SDL:event:button:y e))
   (signal-set! mouse-last-up (SDL:event:button:button e))))
