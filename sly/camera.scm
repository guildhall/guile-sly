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
;; A view to a scene graph.
;;
;;; Code:

(define-module (sly camera)
  #:use-module (srfi srfi-9)
  #:use-module (gl)
  #:use-module (sly wrappers gl)
  #:use-module (sly color)
  #:use-module (sly rect)
  #:use-module (sly signal)
  #:use-module (sly transform)
  #:export (make-camera
            orthographic-camera
            camera?
            camera-scene
            camera-location
            camera-projection
            camera-viewport
            camera-clear-flags
            camera-clear-color
            camera-before-draw-handler camera-after-draw-handler
            call-with-camera))

(define-record-type <camera>
  (%make-camera location projection viewport clear-flags clear-color
                before-draw-handler after-draw-handler)
  camera?
  (location camera-location)
  (projection camera-projection)
  (viewport camera-viewport)
  (clear-flags camera-clear-flags)
  (clear-color camera-clear-color)
  (before-draw-handler camera-before-draw-handler)
  (after-draw-handler camera-after-draw-handler))

(define* (make-camera location projection viewport
                      #:optional #:key
                      (clear-flags '(color-buffer depth-buffer))
                      (clear-color black)
                      before-draw after-draw)
  (%make-camera location projection viewport clear-flags clear-color
                before-draw after-draw))

(define* (orthographic-camera width height
                              #:optional #:key
                              (z-near 0) (z-far 1)
                              (viewport (make-rect 0 0 width height))
                              #:allow-other-keys #:rest rest)
  "Return a camera that uses an orthographic (2D) projection of size
WIDTH x HEIGHT.  Optionally, z-axis clipping planes Z-NEAR and Z-FAR
can be specified, but default to 0 and 1, respectively.  By default,
the camera's VIEWPORT uses the same dimensions as the projection,
which is convenient if the dimensions are in pixels.  Like
'make-camera', custom CLEAR-COLOR and CLEAR-FLAGS can be specified."
  (apply make-camera
         identity-transform
         (orthographic-projection 0 width 0 height z-near z-far)
         viewport
         rest))

;; guile-opengl's clear-buffer-mask does not work with symbols, only
;; syntax.
(define (clear-buffer-mask . flags)
  (apply logior
   (map (lambda (flag)
          (assq-ref '((depth-buffer . 256)
                      (accum-buffer . 512)
                      (stencil-buffer . 1024)
                      (color-buffer . 16384)
                      (coverage-buffer-bit-nv . 32768))
                    flag))
        flags)))

(define (clear-camera camera)
  "Define viewport and clear it."
  (let ((vp (camera-viewport camera))
        (c (camera-clear-color camera)))
    (gl-viewport (rect-x vp)
                 (rect-y vp)
                 (rect-width vp)
                 (rect-height vp))
    ;; Restrict gl-clear to the viewport.
    (gl-scissor (rect-x vp)
                (rect-y vp)
                (rect-width vp)
                (rect-height vp))
    (gl-clear-color (color-r c)
                    (color-g c)
                    (color-b c)
                    (color-a c))
    (gl-clear (apply clear-buffer-mask (camera-clear-flags camera)))))

(define (run-handler camera getter)
  (let ((handler (getter camera)))
    (when (procedure? handler)
      (handler))))

;; emacs: (put 'call-with-camera 'scheme-indent-function 1)
(define (call-with-camera camera proc)
  "Setup CAMERA state and apply PROC."
  ;; Enable texturing, alpha blending, face culling, depth
  ;; and scissor tests.
  (gl-enable (enable-cap texture-2d))
  (gl-enable (enable-cap blend))
  (gl-enable (enable-cap cull-face))
  (gl-enable (enable-cap depth-test))
  (gl-enable (enable-cap scissor-test))
  (set-gl-blend-function (blending-factor-src src-alpha)
                         (blending-factor-dest one-minus-src-alpha))
  (run-handler camera camera-before-draw-handler)
  (clear-camera camera)
  (signal-let ((projection (camera-projection camera))
               (location (camera-location camera)))
    (proc projection location))
  (run-handler camera camera-after-draw-handler)
  (gl-disable (enable-cap texture-2d))
  (gl-disable (enable-cap blend))
  (gl-disable (enable-cap cull-face))
  (gl-disable (enable-cap depth-test))
  (gl-disable (enable-cap scissor-test)))
