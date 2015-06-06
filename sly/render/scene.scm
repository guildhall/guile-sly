;;; Sly
;;; Copyright (C) 2015 David Thompson <davet@gnu.org>
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
;; Scene data type.
;;
;;; Code:

(define-module (sly render scene)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (sly math transform)
  #:use-module (sly render camera)
  #:use-module (sly render context)
  #:use-module (sly render framebuffer)
  #:use-module (sly render model)
  #:use-module (sly render sprite)
  #:export (make-scene
            scene
            scene?
            scene-camera
            scene-model
            scene-framebuffer
            draw-scene
            scene->sprite))

(define-record-type <scene>
  (%make-scene camera model framebuffer)
  scene?
  (camera scene-camera)
  (model scene-model)
  (framebuffer scene-framebuffer))

(define* (make-scene camera model #:optional (framebuffer null-framebuffer))
  "Create a new scene that views MODEL from the perspective of CAMERA.
FRAMEBUFFER specifies where the scene will be drawn to.  By default, a
scene is drawn to directly to the OpenGL window."
  (%make-scene camera model framebuffer))

(define scene make-scene)

(define (draw-scene scene context)
  "Render SCENE with the given rendering CONTEXT."
  (match scene
    (($ <scene> camera model framebuffer)
     (with-transform-excursion context
       (render-context-transform-identity! context)
       (let ((view (render-context-transform context)))
         (transform*! view
                      (camera-location camera)
                      (camera-projection camera))
         (with-transform-excursion context
           (render-context-transform-identity! context)
           (set-render-context-framebuffer! context framebuffer)
           (set-render-context-viewport! context (camera-viewport camera))
           (clear-viewport (camera-viewport camera))
           (draw-model model view context)))))))

(define* (scene->sprite scene #:key (anchor 'center))
  "Create a sprite that renders the framebuffer texture for SCENE."
  (model-inherit (make-sprite (framebuffer-texture (scene-framebuffer scene))
                              #:anchor anchor)
                 #:sub-scene scene))
