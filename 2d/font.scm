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
;; Font rendering.
;;
;;; Code:

(define-module (2d font)
  #:use-module (figl gl)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module ((sdl ttf) #:prefix SDL:)
  #:use-module (figl gl)
  #:use-module (figl contrib packed-struct)
  #:use-module (2d color)
  #:use-module (2d config)
  #:use-module (2d shader)
  #:use-module (2d signal)
  #:use-module (2d texture)
  #:use-module (2d vector2)
  #:use-module (2d window)
  #:use-module (2d wrappers gl)
  #:export (load-font
            load-default-font
            font?
            font-point-size
            make-label
            label?
            label-font
            label-text
            label-position
            label-color
            draw-label))

(SDL:ttf-init)

;;;
;;; Font
;;;

(define-record-type <font>
  (make-font ttf point-size)
  font?
  (ttf font-ttf)
  (point-size font-point-size))

(define (load-font filename point-size)
  "Load the TTF font in FILENAME with the given POINT-SIZE."
  (if (file-exists? filename)
      (make-font (SDL:load-font filename point-size) point-size)
      (error "File not found!" filename)))

(define* (load-default-font #:optional (point-size 12))
  "Load the guile-2d default TTF font.  POINT-SIZE is an optional
argument with a default value of 12."
  (load-font (string-append %pkgdatadir "/fonts/DejaVuSans.ttf") point-size))

(define (render-text font text)
  "Return a new texture with TEXT rendered using FONT."
  (let* ((surface (SDL:render-utf8 (font-ttf font) text
                                   (SDL:make-color 255 255 255) #t))
         (pixels (SDL:surface-pixels surface))
         (texture-id (gl-generate-texture)))
    (with-gl-bind-texture (texture-target texture-2d) texture-id
      (gl-texture-parameter (texture-target texture-2d)
                            (texture-parameter-name texture-min-filter)
                            (texture-min-filter nearest))
      (gl-texture-parameter (texture-target texture-2d)
                            (texture-parameter-name texture-mag-filter)
                            (texture-mag-filter nearest))
      (gl-texture-image-2d (texture-target texture-2d)
                           0
                           (pixel-format rgba)
                           (SDL:surface:w surface)
                           (SDL:surface:h surface)
                           0
                           (version-1-2 bgra)
                           (color-pointer-type unsigned-byte)
                           pixels))
    (make-texture texture-id #f
                  (SDL:surface:w surface)
                  (SDL:surface:h surface)
                  0 0 1 1)))

(define-record-type <label>
  (%make-label font text position anchor color texture vertices)
  label?
  (font label-font)
  (text label-text)
  (position label-position)
  (anchor label-anchor)
  (color label-color)
  (texture label-texture)
  (vertices label-vertices))

(define (make-label-vertices texture)
  "Return a packed array of vertices for TEXTURE."
  (let ((vertices (make-packed-array texture-vertex 4)))
    (pack-texture-vertices vertices 0
                           (texture-width texture)
                           (texture-height texture)
                           (texture-s1 texture)
                           (texture-t1 texture)
                           (texture-s2 texture)
                           (texture-t2 texture))
    vertices))

(define* (make-label font text position #:optional #:key
                     (color white) (anchor 'top-left))
  "Return a new label containing the string TEXT rendered with FONT at
the given position.  Optional arguments are COLOR with a default of
white and ANCHOR with a default of 'top-left."
  (let* ((texture (render-text font text))
         (vertices (make-label-vertices texture))
         (anchor (anchor-texture texture anchor)))
    (%make-label font text position anchor color texture vertices)))

(define font-shader
  (make-shader-program
   (load-vertex-shader (string-append %pkgdatadir
                                      "/shaders/font-vertex.glsl"))
   (load-fragment-shader (string-append %pkgdatadir
                                        "/shaders/font-fragment.glsl"))))

(define (draw-label label)
  "Draw LABEL on the screen."
  (with-shader-program font-shader
    (uniforms ((projection (signal-ref window-projection))
               (position (label-position label))
               (anchor (label-anchor label))
               (color (label-color label)))
      (draw-texture-vertices (label-texture label) (label-vertices label) 1)))
  *unspecified*)
