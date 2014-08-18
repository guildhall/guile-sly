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
;; Sprites are typically the most important part of a 2D game. This
;; module provides sprites as an abstraction around OpenGL textures.
;;
;;; Code:

(define-module (sly sprite)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gl)
  #:use-module (gl contrib packed-struct)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (sly config)
  #:use-module (sly agenda)
  #:use-module (sly helpers)
  #:use-module (sly math)
  #:use-module (sly mesh)
  #:use-module (sly shader)
  #:use-module (sly signal)
  #:use-module (sly texture)
  #:export (make-sprite
            make-animated-sprite))

;;;
;;; Sprites
;;;

(define* (make-sprite texture #:optional #:key (shader (load-default-shader)))
  (let* ((half-width (half (texture-width texture)))
         (half-height (half (texture-height texture)))
         (s1 (texture-s1 texture))
         (t1 (texture-t1 texture))
         (s2 (texture-s2 texture))
         (t2 (texture-t2 texture)))
    (make-mesh
     #:shader shader
     #:texture texture
     #:indices #(0 3 2 0 2 1)
     #:data `(("position" ,(vector
                            (vector (- half-width) (- half-height) 0)
                            (vector half-width (- half-height) 0)
                            (vector half-width half-height 0)
                            (vector (- half-width) half-height 0)))
              ("tex" ,(vector
                       (vector s1 t1)
                       (vector s2 t1)
                       (vector s2 t2)
                       (vector s1 t2)))))))

(define* (make-animated-sprite textures frame-duration #:optional #:key
                               (loop? #t)
                               (shader (load-default-shader)))
  "Return a signal that iterates through the list TEXTURES and
displays them each for FRAME-DURATION ticks.  The optional LOOP? flag
specifies if the animation should play once or indefinitely.
Optionally, a SHADER can be specified, otherwise the default mesh
shader is used."
  (let ((frames (map (cut make-sprite <> #:shader shader) textures)))
    (signal-generator
     (define (animate)
       (for-each (lambda (frame)
                   (yield frame)
                   (wait frame-duration))
                 frames))

     (if loop?
         (forever (animate))
         (animate)))))
