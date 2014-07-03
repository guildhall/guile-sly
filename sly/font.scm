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
;; Font rendering.
;;
;;; Code:

(define-module (sly font)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module ((sdl ttf) #:prefix SDL:)
  #:use-module (gl)
  #:use-module (gl contrib packed-struct)
  #:use-module (sly color)
  #:use-module (sly config)
  #:use-module (sly shader)
  #:use-module (sly signal)
  #:use-module (sly texture)
  #:use-module (sly vector)
  #:use-module (sly window)
  #:use-module (sly wrappers gl)
  #:export (enable-fonts
            load-font
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

;;;
;;; Font
;;;

(define font-shader #f)

(define (enable-fonts)
  (SDL:ttf-init)
  (set! font-shader
        (load-shader-program
         (string-append %pkgdatadir
                        "/shaders/font-vertex.glsl")
         (string-append %pkgdatadir
                        "/shaders/font-fragment.glsl"))))

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
  "Load the Sly default TTF font.  POINT-SIZE is an optional
argument with a default value of 12."
  (load-font (string-append %pkgdatadir "/fonts/DejaVuSans.ttf") point-size))

(define (render-text font text)
  "Return a new texture with TEXT rendered using FONT."
  ;; An empty string will result in a surface value of #f, in which we
  ;; want to abort the texture creation process.
  (and-let* ((surface (SDL:render-utf8 (font-ttf font) text
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
  (text %label-text)
  (position %label-position)
  (anchor label-anchor)
  (color %label-color)
  (texture label-texture)
  (vertices label-vertices))

(define label-text (compose signal-ref-maybe %label-text))
(define label-position (compose signal-ref-maybe %label-position))
(define label-color (compose signal-ref-maybe %label-color))

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
         (vertices (and texture (make-label-vertices texture)))
         (anchor (if texture (anchor-texture texture anchor) #(0 0))))
    (%make-label font text position anchor color texture vertices)))

(define (draw-label label)
  "Draw LABEL on the screen."
  (when (label-texture label)
    (with-shader-program font-shader
      (uniforms ((projection (signal-ref window-projection))
                 (position (label-position label))
                 (anchor (label-anchor label))
                 (color (label-color label)))
        (draw-texture-vertices (label-texture label) (label-vertices label) 1))))
  *unspecified*)
