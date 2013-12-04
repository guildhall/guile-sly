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
;; Window management.
;;
;;; Code:

(define-module (2d window)
  #:use-module (srfi srfi-9)
  #:use-module (figl gl)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module ((sdl mixer) #:prefix SDL:)
  #:use-module (2d game)
  #:use-module (2d signals)
  #:use-module (2d vector2)
  #:export (<window>
            make-window
            window?
            window-title
            window-resolution
            window-fullscreen?
            window-size
            open-window
            close-window
            with-window))

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

(define window-size (make-signal #:init (vector2 0 0)))

(register-event-handler
 'video-resize
 (lambda (e)
   (signal-set! window-size
                (vector2 (SDL:event:resize:w e)
                         (SDL:event:resize:h e)))))

(define* (open-window window)
  "Open the game window using the settings in WINDOW."
  (let ((flags (if (window-fullscreen? window) '(opengl fullscreen) 'opengl))
        (width (vx (window-resolution window)))
        (height (vy (window-resolution window))))
    ;; Initialize everything
    (SDL:enable-unicode #t)
    (SDL:init 'everything)
    (SDL:open-audio)
    ;; Open SDL window in OpenGL mode.
    (SDL:set-video-mode width height 24 flags)
    (SDL:set-caption (window-title window))
    ;; Initialize OpenGL orthographic view
    (gl-viewport 0 0 width height)
    (set-gl-matrix-mode (matrix-mode projection))
    (gl-load-identity)
    (gl-ortho 0 width height 0 -1 1)
    (set-gl-matrix-mode (matrix-mode modelview))
    (gl-load-identity)
    ;; Enable texturing and alpha blending
    (gl-enable (enable-cap texture-2d))
    (gl-enable (enable-cap blend))
    (set-gl-blend-function (blending-factor-src src-alpha)
                           (blending-factor-dest one-minus-src-alpha))))

(define (close-window)
  "Close the currently open window and audio."
  (SDL:close-audio)
  (SDL:quit))

(define-syntax-rule (with-window window body ...)
  (dynamic-wind
    (lambda () (open-window window))
    (lambda () body ...)
    (lambda () (close-window))))

;; Open a window immediately to create an OpenGL context. This allows
;; for textures to be loaded before the program explicitly sets the
;; window settings.
;;
;; TODO: Hide the window initially. This will require SDL 2.0.
(open-window (make-window #:title ""))
