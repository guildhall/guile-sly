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
;; Cameras and viewports.
;;
;;; Code:

(define-module (sly render camera)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (gl)
  #:use-module (gl low-level)
  #:use-module (gl enums)
  #:use-module (sly wrappers gl)
  #:use-module (sly color)
  #:use-module (sly rect)
  #:use-module (sly transform)
  #:export (make-viewport viewport?
            viewport-area viewport-clear-color viewport-clear-flags
            apply-viewport
            make-camera camera?
            camera-location camera-projection camera-viewport
            orthographic-camera))

;;;
;;; Viewport
;;;

(define-record-type <viewport>
  (%make-viewport area clear-color clear-flags)
  viewport?
  (area viewport-area)
  (clear-color viewport-clear-color)
  (clear-flags viewport-clear-flags))

(define* (make-viewport area #:optional #:key (clear-color black)
                        (clear-flags '(color-buffer depth-buffer)))
  "Create a viewport that covers the rectangle AREA of the window.
Fill the viewport with CLEAR-COLOR when clearing the screen.  Clear
the buffers denoted by the list of symbols in CLEAR-FLAGS.  Possible
values for CLEAR-FLAGS are 'color-buffer', 'depth-buffer',
'accum-buffer', and 'stencil-buffer'."
  (%make-viewport area clear-color clear-flags))

(define (clear-buffer-mask flags)
  (apply logior
         ;; Map symbols to OpenGL constants.
         (map (match-lambda
                ('depth-buffer 256)
                ('accum-buffer 512)
                ('stencil-buffer 1024)
                ('color-buffer 16384))
              flags)))

(define (apply-viewport viewport)
  "Set the OpenGL state for VIEWPORT.  Clip rendering to the viewport
area, set the clear color, and clear necessary buffers."
  (gl-enable (enable-cap scissor-test))
  (match (viewport-area viewport)
    (($ <rect> x y width height)
     (gl-viewport x y width height)
     (gl-scissor x y width height)))
  (match (viewport-clear-color viewport)
    (($ <color> r g b a)
     (gl-clear-color r g b a)))
  (gl-clear (clear-buffer-mask (viewport-clear-flags viewport))))

;;;
;;; Camera
;;;

(define-record-type <camera>
  (make-camera location projection viewport)
  camera?
  (location camera-location)
  (projection camera-projection)
  (viewport camera-viewport))

(define* (orthographic-camera width height
                              #:optional #:key
                              (z-near 0) (z-far 1)
                              (viewport (make-viewport
                                         (make-rect 0 0 width height))))
  "Create a camera object that uses an orthographic (2D) projection of
size WIDTH x HEIGHT.  Optionally, z-axis clipping planes Z-NEAR and
Z-FAR can be specified, but default to 0 and 1, respectively.  By
default, the camera's VIEWPORT is WIDTH x HEIGHT, which is convenient if
the dimensions are measured in pixels."
  (let ((projection (orthographic-projection 0 width 0 height z-near z-far)))
    (make-camera identity-transform projection viewport)))
