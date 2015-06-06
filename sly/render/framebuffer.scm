;;; Sly
;;; Copyright (C) 2014 David Thompson <davet@gnu.org>
;;;
;;; Sly is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Sly is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Render to texture.
;;
;;; Code:

(define-module (sly render framebuffer)
  #:use-module ((system foreign) #:select (bytevector->pointer %null-pointer))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (gl)
  #:use-module (gl low-level)
  #:use-module (gl enums)
  #:use-module (sly wrappers gl)
  #:export (make-framebuffer
            framebuffer?
            framebuffer-id framebuffer-renderbuffer-id
            framebuffer-texture
            null-framebuffer
            apply-framebuffer))

(define (generate-framebuffer)
  "Generate a new OpenGL framebuffer object."
  (let ((bv (u32vector 1)))
    (glGenFramebuffers 1 (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define (generate-renderbuffer)
  "Generate a new OpenGL renderbuffer object."
  (let ((bv (u32vector 1)))
    (glGenRenderbuffers 1 (bytevector->pointer bv))
    (u32vector-ref bv 0)))

(define-record-type <framebuffer>
  (%make-framebuffer id renderbuffer-id texture)
  framebuffer?
  (id framebuffer-id)
  (renderbuffer-id framebuffer-renderbuffer-id)
  (texture framebuffer-texture))

(define null-framebuffer
  (%make-framebuffer 0 0 #f))

(define make-framebuffer
  (let ((draw-buffers (u32vector (version-3-0 color-attachment0))))
    (lambda (width height)
      "Create a new framebuffer that renders to a texture with
dimensions WIDTH x HEIGHT."
      (let ((framebuffer-id (generate-framebuffer))
            (renderbuffer-id (generate-renderbuffer))
            (texture-id (gl-generate-texture)))
        (glBindFramebuffer (version-3-0 framebuffer) framebuffer-id)
        ;; Setup texture that will be attached to the framebuffer.
        (with-gl-bind-texture (texture-target texture-2d) texture-id
          (gl-texture-parameter (texture-target texture-2d)
                                (texture-parameter-name texture-min-filter)
                                (texture-min-filter nearest))
          (gl-texture-parameter (texture-target texture-2d)
                                (texture-parameter-name texture-mag-filter)
                                (texture-mag-filter nearest))
          (gl-texture-image-2d (texture-target texture-2d)
                               0
                               (pixel-format rgb)
                               width
                               height
                               0
                               (pixel-format rgb)
                               (color-pointer-type unsigned-byte)
                               %null-pointer)
          ;; Setup depth buffer.
          (glBindRenderbuffer (version-3-0 renderbuffer)
                              renderbuffer-id)
          (glRenderbufferStorage (version-3-0 renderbuffer)
                                 (pixel-format depth-component)
                                 width
                                 height)
          (glFramebufferRenderbuffer (version-3-0 framebuffer)
                                     (arb-framebuffer-object depth-attachment)
                                     (version-3-0 renderbuffer)
                                     renderbuffer-id)
          ;; Setup framebuffer.
          (glFramebufferTexture2D (version-3-0 framebuffer)
                                  (version-3-0 color-attachment0)
                                  (texture-target texture-2d)
                                  texture-id
                                  0)
          (glDrawBuffers 1 (bytevector->pointer draw-buffers))
          ;; Check for errors.
          (unless (= (glCheckFramebufferStatus (version-3-0 framebuffer))
                     (version-3-0 framebuffer-complete))
            (error "Framebuffer cannot be created")))
        ;; Clean up.
        (glBindRenderbuffer (version-3-0 renderbuffer) 0)
        (glBindFramebuffer (version-3-0 framebuffer) 0)
        ;; Build high-level framebuffer object.
        (let ((texture ((@@ (sly render texture) %make-texture)
                        texture-id #f width height 0 0 1 1)))
          (%make-framebuffer framebuffer-id renderbuffer-id texture))))))

(define (apply-framebuffer framebuffer)
  (glBindFramebuffer (version-3-0 framebuffer)
                     (framebuffer-id framebuffer)))
