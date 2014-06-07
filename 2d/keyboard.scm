;;; guile-2d
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

(define-module (2d keyboard)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (2d event)
  #:use-module (2d signal)
  #:use-module (2d vector)
  #:export (key-press-hook
            key-release-hook
            key-last-down
            key-last-up
            key-down?
            key-directions
            key-arrows
            key-wasd))

(define key-press-hook (make-hook 2))

(register-event-handler
 'key-down
 (lambda (e)
   (run-hook key-press-hook
             (SDL:event:key:keysym:sym e)
             (SDL:event:key:keysym:unicode e))))

(define-signal key-last-down
  (hook->signal key-press-hook 'none
                (lambda (key unicode)
                  key)))

(define key-release-hook (make-hook 2))

(register-event-handler
 'key-up
 (lambda (e)
   (run-hook key-release-hook
             (SDL:event:key:keysym:sym e)
             (SDL:event:key:keysym:unicode e))))

(define-signal key-last-up
  (hook->signal key-release-hook 'none
                (lambda (key unicode)
                  key)))

(define (key-down? key)
  "Create a signal for the state of KEY. The signal value is #t when
KEY is pressed or #f otherwise."
  (define (same-key? other-key)
    (eq? key other-key))
  (define (key-filter value signal)
    (signal-constant value (signal-filter same-key? #f signal)))
  (signal-merge (key-filter #f key-last-up)
                (key-filter #t key-last-down)))

(define (key-directions up down left right)
  (signal-map (lambda (up? down? left? right?)
                (vector (+ (if left? -1 0)
                           (if right? 1 0))
                        (+ (if up? -1 0)
                           (if down? 1 0))))
              (key-down? up)
              (key-down? down)
              (key-down? left)
              (key-down? right)))

(define-signal key-arrows (key-directions 'up 'down 'left 'right))
(define-signal key-wasd (key-directions 'w 's 'a 'd))
