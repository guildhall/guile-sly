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
;; Window management.
;;
;;; Code:

(define-module (2d window)
  #:use-module (srfi srfi-9)
  #:use-module (gl)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module ((sdl mixer) #:prefix SDL:)
  #:use-module (2d event)
  #:use-module (2d signal)
  #:use-module (2d transform)
  #:use-module (2d vector2)
  #:export (make-window
            window?
            window-title
            window-resolution
            window-fullscreen?
            window-width
            window-height
            window-size
            window-projection
            open-window
            close-window
            with-window
            window-resize-hook
            window-close-hook))

(define-record-type <window>
  (%make-window title resolution fullscreen?)
  window?
  (title window-title)
  (resolution window-resolution)
  (fullscreen? window-fullscreen?))

(define* (make-window #:optional #:key
                      (title "Guile-2D Window")
                      (resolution (vector2 640 480))
                      (fullscreen? #f))
  (%make-window title resolution fullscreen?))

(define window-resize-hook (make-hook 2))

(register-event-handler
 'video-resize
 (lambda (e)
   (run-hook window-resize-hook
             (SDL:event:resize:w e)
             (SDL:event:resize:h e))))

(define-signal window-size
  (hook->signal window-resize-hook
                null-vector2
                (lambda (width height)
                  (vector2 width height))))
(define-signal window-width (signal-map vx window-size))
(define-signal window-height (signal-map vy window-size))

(define-signal window-projection
  (signal-map (lambda (size)
                (if (or (zero? (vx size)) (zero? (vy size)))
                    identity-transform
                    (orthographic-projection 0 (vx size) 0 (vy size) -1 1)))
              window-size))

(define window-close-hook (make-hook))

(register-event-handler
 'quit
 (lambda (e)
   (run-hook window-close-hook)))

(define* (open-window #:optional (window (make-window #:title "")))
  "Open the game window using the settings in WINDOW."
  (let ((flags (if (window-fullscreen? window) '(opengl fullscreen) 'opengl))
        (width (vx (window-resolution window)))
        (height (vy (window-resolution window))))
    (signal-set! window-size (vector2 width height))
    ;; Initialize everything
    (SDL:enable-unicode #t)
    (SDL:init 'everything)
    ;; Open SDL window in OpenGL mode.
    (SDL:set-video-mode width height 24 flags)
    (SDL:set-caption (window-title window))
    ;; Enable texturing and alpha blending
    (gl-enable (enable-cap texture-2d))
    (gl-enable (enable-cap blend))
    (set-gl-blend-function (blending-factor-src src-alpha)
                           (blending-factor-dest one-minus-src-alpha))))

(define (close-window)
  "Close the currently open window and audio."
  (SDL:quit))

(define-syntax-rule (with-window window body ...)
  (dynamic-wind
    (lambda () (open-window window))
    (lambda () body ...)
    (lambda () (close-window))))
