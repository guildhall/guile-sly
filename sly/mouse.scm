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

;;; Commentary:
;;
;; Keyboard signals.
;;
;;; Code:

(define-module (sly mouse)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (sly event)
  #:use-module (sly signal)
  #:use-module (sly math vector)
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

(register-event-handler
 'mouse-motion
 (lambda (e)
   (run-hook mouse-move-hook
             (SDL:event:motion:x e)
             (SDL:event:motion:y e))))

(define-signal mouse-position
  (hook->signal mouse-move-hook
                (vector2 0 0)
                vector2))

(define-signal mouse-x (signal-map vx mouse-position))
(define-signal mouse-y (signal-map vy mouse-position))

(define mouse-press-hook (make-hook 3))

(register-event-handler
 'mouse-button-down
 (lambda (e)
   (run-hook mouse-press-hook
             (SDL:event:button:button e)
             (SDL:event:button:x e)
             (SDL:event:button:y e))))

(define-signal mouse-last-down
  (hook->signal mouse-press-hook
                'none
                (lambda (button x y)
                  button)))

(define mouse-click-hook (make-hook 3))

(register-event-handler
 'mouse-button-up
 (lambda (e)
   (run-hook mouse-click-hook
             (SDL:event:button:button e)
             (SDL:event:button:x e)
             (SDL:event:button:y e))))

(define-signal mouse-last-up
  (hook->signal mouse-click-hook
                'none
                (lambda (button x y)
                  button)))

(define (mouse-down? button)
  "Create a signal for the state of BUTTON. Value is #t when mouse
button is pressed or #f otherwise."
  (define (same-button? other-button)
    (eq? button other-button))

  (define (button-filter value signal)
    (signal-constant value (signal-filter #f same-button? signal)))

  (signal-merge (button-filter #f mouse-last-up)
                (button-filter #t mouse-last-down)))
