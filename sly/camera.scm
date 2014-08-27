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
  #:use-module (sly scene)
  #:use-module (sly signal)
  #:use-module (sly transform)
  #:export (make-camera
            camera?
            camera-scene
            camera-location
            camera-projection
            camera-viewport
            camera-clear-flags
            camera-clear-color
            draw-camera))

(define-record-type <camera>
  (%make-camera scene location projection viewport clear-flags clear-color)
  camera?
  (scene camera-scene)
  (location camera-location)
  (projection camera-projection)
  (viewport camera-viewport)
  (clear-flags camera-clear-flags)
  (clear-color camera-clear-color))

(define* (make-camera scene location projection viewport
                      #:optional #:key
                      (clear-flags '(color-buffer depth-buffer))
                      (clear-color black))
  (%make-camera scene location projection viewport clear-flags clear-color))

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

(define (draw-camera camera alpha)
  "Draw SCENE from the perspective of CAMERA with interpolation factor
ALPHA."
  (clear-camera camera)
  (signal-let ((scene (camera-scene camera))
               (projection (camera-projection camera))
               (location (camera-location camera)))
    (draw-scene-node scene alpha (transform* projection location))))
