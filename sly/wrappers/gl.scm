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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-4)
  #:use-module ((system foreign) #:select (bytevector->pointer))
  #:use-module ((gl low-level) #:renamer (symbol-prefix-proc '%))
  #:use-module (gl enums)
  #:use-module (gl runtime)
  #:use-module (gl types))

(re-export (%glClearColor . gl-clear-color)
           (%glScissor . gl-scissor))

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

(define gl-current-texture
  (let ((binding-map (list (cons (texture-target texture-1d)
                                 (get-p-name texture-binding-1d))
                           (cons (texture-target texture-2d)
                                 (get-p-name texture-binding-2d))
                           (cons (texture-target texture-3d-ext)
                                 (get-p-name texture-binding-3d)))))
    (lambda (target)
      "Return the id of the currently bound texture."
      (let ((bv (make-s32vector 1)))
        (%glGetIntegerv (assoc-ref binding-map target)
                        (bytevector->pointer bv))
        (s32vector-ref bv 0)))))

;; emacs: (put 'with-gl-bind-texture 'scheme-indent-function 2)
(define-syntax-rule (with-gl-bind-texture target id body ...)
  (let ((old-id (gl-current-texture target)))
    (if (= id old-id)
        (begin body ...)
        (begin
          (%glBindTexture target id)
          body ...
          (%glBindTexture target old-id)))))

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

(define-gl-procedure (glDeleteVertexArrays (n GLsizei)
                                           (arrays GLuint-*)
                                           -> void)
  "Delete vertex array objects.")

(define-gl-procedure (glBindVertexArray (array GLuint)
                                        -> void)
  "Bind vertex array object ARRAY.")

(export glGenVertexArrays
        glDeleteVertexArrays
        glBindVertexArray)

(define-syntax-rule (with-gl-client-state state body ...)
  (begin
    (gl-enable-client-state state)
    body ...
    (gl-disable-client-state state)))

(export with-gl-client-state)

;;;
;;; Framebuffers
;;;

(define-gl-procedure (glGenFramebuffers (n GLsizei)
                                        (ids GLuint-*)
                                        -> void)
  "Generate framebuffer object names.")

(define-gl-procedure (glDeleteFramebuffers (n GLsizei)
                                           (framebuffers GLuint-*)
                                           -> void)
  "Delete framebuffer objects.")

(define-gl-procedure (glBindFramebuffer (target GLenum)
                                        (framebuffer GLuint)
                                        -> void)
  "Bind a framebuffer to a framebuffer target.")

(define-gl-procedure (glFramebufferTexture2D (target GLenum)
                                             (attachment GLenum)
                                             (textarget GLenum)
                                             (texture GLuint)
                                             (level GLint)
                                             -> void)
  "Attach a level of a texture object as a logical buffer to the
currently bound framebuffer object.")

(define-gl-procedure (glCheckFramebufferStatus (target GLenum)
                                               -> GLenum)
  "Return the framebuffer completeness status of a framebuffer
object.")

(define-gl-procedure (glGenRenderbuffers (n GLsizei)
                                         (ids GLuint-*)
                                         -> void)
  "Generate renderbuffer object names.")

(define-gl-procedure (glDeleteRenderbuffers (n GLsizei)
                                            (renderbuffers GLuint-*)
                                            -> void)
  "Delete renderbuffer objects.")

(define-gl-procedure (glBindRenderbuffer (target GLenum)
                                         (renderbuffer GLuint)
                                         -> void)
  "Bind a named renderbuffer object.")

(define-gl-procedure (glRenderbufferStorage (target GLenum)
                                            (internalformat GLenum)
                                            (width GLsizei)
                                            (height GLsizei)
                                            -> void)
  "Create and initialize a renderbuffer object's data store.")

(define-gl-procedure (glFramebufferRenderbuffer (target GLenum)
                                                (attachment GLenum)
                                                (renderbuffertarget GLenum)
                                                (renderbuffer GLuint)
                                                -> void)
  "Attach a renderbuffer object to a framebuffer object.")

(export glGenFramebuffers
        glDeleteFramebuffers
        glBindFramebuffer
        glFramebufferTexture2D
        glCheckFramebufferStatus
        glGenRenderbuffers
        glDeleteRenderbuffers
        glBindRenderbuffer
        glRenderbufferStorage
        glFramebufferRenderbuffer)
