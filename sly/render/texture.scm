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
;; Textures and texture regions are high level wrappers over OpenGL
;; textures.
;;
;;; Code:

(define-module (sly render texture)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (gl)
  #:use-module (gl low-level)
  #:use-module (gl contrib packed-struct)
  #:use-module (sly render color)
  #:use-module (sly utils)
  #:use-module (sly math vector)
  #:use-module (sly wrappers gl)
  #:use-module (sly wrappers freeimage)
  #:export (make-texture
            make-texture-region
            bytevector->texture
            load-texture
            texture?
            texture-region?
            texture-id
            texture-width
            texture-height
            texture-s1
            texture-t1
            texture-s2
            texture-t2
            anchor-texture
            texture-vertex
            pack-texture-vertices
            draw-texture-vertices
            apply-texture
            with-texture))

;;;
;;; Textures
;;;

;; The <texture> object is a simple wrapper around an OpenGL texture
;; id.
(define-record-type <texture>
  (%make-texture id parent width height s1 t1 s2 t2)
  texture?
  (id texture-id)
  (parent texture-parent)
  (width texture-width)
  (height texture-height)
  (s1 texture-s1)
  (t1 texture-t1)
  (s2 texture-s2)
  (t2 texture-t2))

(define (texture-region? texture)
  "Return #t if TEXTURE has a parent texture."
  (texture? (texture-parent texture)))

(define (make-texture id parent width height s1 t1 s2 t2)
  "Create a new texture object. ID is the OpenGL texture id. PARENT is
a texture object (if this texture only represents a region of another
texture) or #f. WIDTH and HEIGHT are the texture dimensions in
pixels. S1, T1, S2, and T2 are the OpenGL texture coordinates
representing the area of the texture that will be rendered."
  (let ((texture (%make-texture id parent width height s1 t1 s2 t2)))
    (texture-guardian texture)
    texture))

(define (make-texture-region texture x y width height)
  "Creates new texture region object. TEXTURE is the region's parent
texture. X, Y, WIDTH, and HEIGHT represent the region of the texture
that will be rendered, in pixels."
  (let* ((w (texture-width texture))
         (h (texture-height texture)))
    (make-texture (texture-id texture)
                  texture
                  width
                  height
                  (/ x w)
                  (/ y h)
                  (/ (+ x width) w)
                  (/ (+ y height) h))))

;; Use a guardian and an after GC hook that ensures that OpenGL
;; textures are deleted when texture objects are GC'd.
(define-guardian texture-guardian
  (lambda (texture)
    ;; Do not reap texture regions
    (unless (texture-region? texture)
      (gl-delete-texture (texture-id texture)))))

(define* (bytevector->texture pixels width height min-filter mag-filter
                              #:optional (format (pixel-format rgba)))
  "Translate the bytevector PIXELS into an OpenGL texture with
dimensions WIDTHxHEIGHT where each pixel corresponds to the given
OpenGL pixel FORMAT.  The generated textured uses MIN-FILTER for
downscaling and MAG-FILTER for upscaling."
  (let ((texture-id (gl-generate-texture)))
    (with-gl-bind-texture (texture-target texture-2d) texture-id
      (gl-texture-parameter (texture-target texture-2d)
                            (texture-parameter-name texture-min-filter)
                            (match min-filter
                              ('nearest (texture-min-filter nearest))
                              ('linear (texture-min-filter linear))))
      (gl-texture-parameter (texture-target texture-2d)
                            (texture-parameter-name texture-mag-filter)
                            (match mag-filter
                              ('nearest (texture-mag-filter nearest))
                              ('linear (texture-mag-filter linear))))
      (gl-texture-image-2d (texture-target texture-2d)
                           0 (pixel-format rgba) width height 0 format
                           (color-pointer-type unsigned-byte)
                           pixels))
    (make-texture texture-id #f width height 0 0 1 1)))

(define (bitmap->texture bitmap min-filter mag-filter)
  "Translates a freeimage bitmap into an OpenGL texture."
  (bytevector->texture (freeimage-get-bits bitmap)
                       (freeimage-get-width bitmap)
                       (freeimage-get-height bitmap)
                       min-filter mag-filter
                       (version-1-2 bgra)))

(define (load-bitmap filename)
  ;; Throw an error if image file does not exist or else we will
  ;; segfault later.
  (unless (file-exists? filename)
    (throw 'image-not-found filename))
  ;; Load image and convert it to 32 bit color.
  (let* ((image-type (freeimage-get-file-type filename))
         (bitmap (freeimage-load image-type filename))
         (32bit-bitmap (freeimage-convert-to-32-bits bitmap)))
    (freeimage-unload bitmap)
    32bit-bitmap))

(define* (load-texture file-name #:optional #:key
                       (min-filter 'nearest) (mag-filter 'nearest))
  "Load a texture from an image file at FILENAME.  MIN-FILTER and
MAG-FILTER describe the method that should be used for minification
and magnification.  Valid values are 'nearest and 'linear.  By
default, 'nearest is used."
  (let* ((bitmap (load-bitmap file-name))
         (texture (bitmap->texture bitmap min-filter mag-filter)))
    (freeimage-unload bitmap)
    texture))

(define (anchor-texture texture anchor)
  "Translate ANCHOR into a vector that represents the desired centtral
point for TEXTURE.  Valid values for ANCHOR are: 'center, 'top-left,
'top-right, 'bottom-left, 'bottom-right, 'top-center, 'bottom-center,
or any 2D vector.  Passing a 2D vector will simply cause the same
vector to be returned."
  (let ((w (texture-width texture))
        (h (texture-height texture)))
    (match anchor
      ((? vector2? anchor)
       anchor)
      ('center
       (vector2 (/ w 2)
                (/ h 2)))
      ('top-left
       (vector2 0 0))
      ('top-right
       (vector2 w 0))
      ('bottom-left
       (vector2 0 h))
      ('bottom-right
       (vector2 w h))
      ('top-center
       (vector2 (/ w 2) 0))
      ('bottom-center
       (vector2 (/ w 2) h))
      (_ (error "Invalid anchor type: " anchor)))))

;;;
;;; Texture Vertices
;;;

(define-packed-struct texture-vertex
  ;; Position
  (x float)
  (y float)
  ;; Texture Coordinates
  (s float)
  (t float))

(define texture-vertex-size (packed-struct-size texture-vertex))
(define x-offset (packed-struct-offset texture-vertex x))
(define s-offset (packed-struct-offset texture-vertex s))

(define (pack-texture-vertices vertices offset width height s1 t1 s2 t2)
  ;; Vertices go counter clockwise, starting from the top-left
  ;; corner.
  (pack vertices offset texture-vertex 0 0 s1 t1)
  (pack vertices (+ offset 1) texture-vertex 0 height s1 t2)
  (pack vertices (+ offset 2) texture-vertex width height s2 t2)
  (pack vertices (+ offset 3) texture-vertex width 0 s2 t1))

(define (apply-texture texture)
  (glBindTexture (texture-target texture-2d) (texture-id texture)))

(define-syntax-rule (with-texture texture body ...)
  (begin
    (apply-texture texture)
    body
    ...
    (glBindTexture (texture-target texture-2d) 0)))

(define (draw-texture-vertices texture vertices size)
  (let ((pointer-type (tex-coord-pointer-type float)))
    (gl-enable-client-state (enable-cap vertex-array))
    (gl-enable-client-state (enable-cap texture-coord-array))
    (with-gl-bind-texture (texture-target texture-2d) (texture-id texture)
      (set-gl-vertex-array pointer-type
                           vertices
                           2
                           #:stride texture-vertex-size
                           #:offset x-offset)
      (set-gl-texture-coordinates-array pointer-type
                                        vertices
                                        #:stride texture-vertex-size
                                        #:offset s-offset)
      (gl-draw-arrays (begin-mode quads) 0 (* size 4)))
    (gl-disable-client-state (enable-cap texture-coord-array))
    (gl-disable-client-state (enable-cap vertex-array))))
