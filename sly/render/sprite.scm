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

(define-module (sly render sprite)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gl)
  #:use-module (gl contrib packed-struct)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (sly render color)
  #:use-module (sly config)
  #:use-module (sly agenda)
  #:use-module (sly utils)
  #:use-module (sly math)
  #:use-module (sly render mesh)
  #:use-module (sly render model)
  #:use-module (sly render texture)
  #:use-module (sly math vector)
  #:export (make-sprite sprite load-sprite))

;;;
;;; Sprites
;;;

(define* (make-sprite texture #:optional #:key
                      (anchor 'center))
  "Return a 2D rectangular mesh that displays the image TEXTURE.  The
size of the mesh is the size of TEXTURE, in pixels."
  (let* ((anchor (anchor-texture texture anchor))
         (x1 (- (floor (vx anchor))))
         (y1 (- (floor (vy anchor))))
         (x2 (+ x1 (texture-width texture)))
         (y2 (+ y1 (texture-height texture)))
         (s1 (texture-s1 texture))
         (t1 (texture-t1 texture))
         (s2 (texture-s2 texture))
         (t2 (texture-t2 texture))
         (mesh (build-mesh #(0 3 2 0 2 1)
                           (vector
                            (vector3 x1 y1 0)
                            (vector3 x2 y1 0)
                            (vector3 x2 y2 0)
                            (vector3 x1 y2 0))
                           (vector
                            (vector2 s1 t1)
                            (vector2 s2 t1)
                            (vector2 s2 t2)
                            (vector2 s1 t2)))))
    (make-model #:texture texture
                #:mesh mesh
                #:depth-test? #f)))

(define sprite make-sprite)

(define* (load-sprite file-name #:key (anchor 'center))
  "Return a sprite mesh for the texture loaded from FILE-NAME."
  (make-sprite (load-texture file-name) #:anchor anchor))
