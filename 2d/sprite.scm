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
;; Sprites are typically the most important part of a 2D game. This
;; module provides sprites as an abstraction around OpenGL textures.
;;
;;; Code:

(define-module (2d sprite)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (gl)
  #:use-module (gl contrib packed-struct)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (2d agenda)
  #:use-module (2d animation)
  #:use-module (2d color)
  #:use-module (2d config)
  #:use-module (2d game)
  #:use-module (2d helpers)
  #:use-module (2d math)
  #:use-module (2d shader)
  #:use-module (2d signal)
  #:use-module (2d texture)
  #:use-module (2d vector2)
  #:use-module (2d window)
  #:use-module (2d wrappers gl)
  #:export (make-sprite
            sprite?
            animated-sprite?
            sprite-drawable
            sprite-position
            set-sprite-drawable!
            set-sprite-position!
            set-sprite-scale!
            set-sprite-rotation!
            set-sprite-color!
            set-sprite-anchor!
            sprite-scale
            sprite-rotation
            sprite-color
            sprite-anchor
            load-sprite
            draw-sprite))

;;;
;;; Sprites
;;;

(define sprite-shader
  (make-shader-program
   (load-vertex-shader (string-append %pkgdatadir
                                      "/shaders/sprite-vertex.glsl"))
   (load-fragment-shader (string-append %pkgdatadir
                                        "/shaders/sprite-fragment.glsl"))))

;; The <sprite> type represents a drawable object (texture,
;; texture-region, animation, etc.) with a given position, scale,
;; rotation, and color.
(define-record-type <sprite>
  (%make-sprite drawable position scale rotation color anchor vertices animator)
  sprite?
  (drawable sprite-drawable set-sprite-drawable!)
  (position sprite-position set-sprite-position!)
  (scale sprite-scale set-sprite-scale!)
  (rotation sprite-rotation set-sprite-rotation!)
  (color sprite-color set-sprite-color!)
  (anchor sprite-anchor %set-sprite-anchor!)
  (vertices sprite-vertices)
  (animator sprite-animator))

(define (update-sprite-vertices! sprite)
  (let ((texture (sprite-texture sprite)))
    (pack-texture-vertices (sprite-vertices sprite)
                           0
                           (texture-width texture)
                           (texture-height texture)
                           (texture-s1 texture)
                           (texture-t1 texture)
                           (texture-s2 texture)
                           (texture-t2 texture))))

(define* (make-sprite drawable #:optional #:key
                      (position (vector2 0 0)) (scale (vector2 1 1))
                      (rotation 0) (color white) (anchor 'center))
  "Create a new sprite object. DRAWABLE is either a texture or
animation object.  All keyword arguments are optional. POSITION is a
vector2 object with a default of (0, 0).  SCALE is a vector2 object
that describes how much DRAWABLE should be strected on the x and y
axes, with a default of 1x scale.  ROTATION is an angle in degrees
with a default of 0.  COLOR is a color object with a default of white.
ANCHOR is either a vector2 that represents the center point of the
sprite, or 'center which will place the anchor at the center of
DRAWABLE.  Sprites are centered by default."
  (let* ((vertices (make-packed-array texture-vertex 4))
         (animator (if (animation? drawable)
                       (make-animator drawable)
                       #f))
         (anchor (anchor-texture (drawable-texture drawable animator) anchor))
         (sprite (%make-sprite drawable position scale rotation color
                               anchor vertices animator)))
    (update-sprite-vertices! sprite)
    sprite))

(define* (load-sprite filename #:optional #:key
                      (position (vector2 0 0)) (scale (vector2 1 1))
                      (rotation 0) (color white) (anchor 'center))
  "Load a sprite from the file at FILENAME. See make-sprite for
optional keyword arguments."
  (make-sprite (load-texture filename)
               #:position position
               #:scale scale
               #:rotation rotation
               #:color color
               #:anchor anchor))

(define (animated-sprite? sprite)
  "Return #t if SPRITE has an animation as its drawable object."
  (animation? (sprite-drawable sprite)))

(define (drawable-texture drawable animator)
  (cond ((texture? drawable)
         drawable)
        ((animation? drawable)
         (animator-texture animator))))

(define (sprite-texture sprite)
  "Return the texture for the SPRITE's drawable object."
  (let ((drawable (sprite-drawable sprite)))
    (drawable-texture (sprite-drawable sprite)
                      (sprite-animator sprite))))

(define (set-sprite-anchor! sprite anchor)
  (%set-sprite-anchor! sprite (anchor-texture (sprite-texture sprite) anchor)))

(define (update-sprite-animator! sprite)
  (animator-update! (sprite-animator sprite))
  (update-sprite-vertices! sprite))

(define (draw-sprite sprite)
  "Render SPRITE to the screen. A sprite batch will be used if one is
currently bound."
  (register-animated-sprite-maybe sprite)
  (with-shader-program sprite-shader
    (uniforms ((position (sprite-position sprite))
               (anchor (sprite-anchor sprite))
               (scale (sprite-scale sprite))
               (rotation (sprite-rotation sprite))
               (color (sprite-color sprite))
               (projection (signal-ref window-projection)))
      (draw-texture-vertices (sprite-texture sprite)
                             (sprite-vertices sprite)
                             1))))

;; A hash table for all of the animated sprites that have been drawn
;; since the last game update.  It is cleared after every game-agenda
;; tick.
(define animated-sprites (make-hash-table))

(define (register-animated-sprite-maybe sprite)
  (when (animated-sprite? sprite)
    (hash-set! animated-sprites sprite sprite)))

(define (update-animated-sprites!)
  "Update all animators for sprites that have been drawn this frame."
  (hash-for-each (lambda (key val)
                   (update-sprite-animator! val))
                 animated-sprites)
  (hash-clear! animated-sprites))

;; Update animated sprites upon every update.
(schedule-each game-agenda update-animated-sprites!)
