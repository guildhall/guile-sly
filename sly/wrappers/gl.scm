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
;; Custom wrappers over low level OpenGL commands that aren't part of
;; guile-opengl.
;;
;;; Code:

(define-module (sly wrappers gl)
  #:use-module ((gl low-level) #:renamer (symbol-prefix-proc '%))
  #:use-module (gl runtime)
  #:use-module (gl types))

;;;
;;; 3.8.1 Texture Image Specification
;;;

(re-export (%glTexImage3D . gl-texture-image-3d)
           (%glTexImage2D . gl-texture-image-2d)
           (%glTexImage1D . gl-texture-image-1d))

;;;
;;; 3.8.2 Alternate Texture Image Specification Commands
;;;

(re-export (%glCopyTexImage2D . gl-copy-texture-image-2d)
           (%glCopyTexImage1D . gl-copy-texture-image-1d)
           (%glCopyTexSubImage3D . gl-copy-texture-sub-image-3d)
           (%glCopyTexSubImage2D . gl-copy-texture-sub-image-2d)
           (%glCopyTexSubImage1D . gl-copy-texture-sub-image-1d)
           (%glTexSubImage3D . gl-texture-sub-image-3d)
           (%glTexSubImage2D . gl-texture-sub-image-2d)
           (%glTexSubImage1D . gl-texture-sub-image-1d))

;;;
;;; 3.8.3 Compressed Texture Images
;;;

(re-export (%glCompressedTexImage1D . gl-compressed-texture-image-1d)
           (%glCompressedTexImage2D . gl-compressed-texture-image-2d)
           (%glCompressedTexImage3D . gl-compressed-texture-image-3d)
           (%glCompressedTexSubImage1D . gl-compressed-texture-sub-image-1d)
           (%glCompressedTexSubImage2D . gl-compressed-texture-sub-image-2d)
           (%glCompressedTexSubImage3D . gl-compressed-texture-sub-image-3d))

;;;
;;; 3.8.4 Texture Parameters
;;;

(re-export  (%glTexParameteri . gl-texture-parameter))

;; emacs: (put 'with-gl-bind-texture 'scheme-indent-function 2)
(define-syntax-rule (with-gl-bind-texture target id body ...)
  (begin
    (%glBindTexture target id)
    body
    ...
    (%glBindTexture target 0)))

(export with-gl-bind-texture)

;;;
;;; Instancing extension
;;;

(define-gl-procedure (glDrawArraysInstanced (mode GLenum)
                                            (first GLint)
                                            (count GLsizei)
                                            (primcount GLsizei)
                                            -> GLboolean)
  "Draw multiple instances of a set of arrays.")

(define-gl-procedure (glVertexAttribDivisor (index GLuint)
                                            (divisor GLuint)
                                            -> void)
  "Modify the rate at which generic vertex attributes advance during
instanced rendering.")

(export glDrawArraysInstanced
        glVertexAttribDivisor)

;;;
;;; VAOs
;;;

(define-gl-procedure (glGenVertexArrays (n GLsizei)
                                        (arrays GLuint-*)
                                        -> void)
  "Generate N vertex arrays.")

(define-gl-procedure (glBindVertexArray (array GLuint)
                                        -> void)
  "Bind vertex array object ARRAY.")

(export glGenVertexArrays
        glBindVertexArray)

(define-syntax-rule (with-gl-client-state state body ...)
  (begin
    (gl-enable-client-state state)
    body ...
    (gl-disable-client-state state)))

(export with-gl-client-state)
