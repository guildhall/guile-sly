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
;; Scene graph
;;
;;; Code:

(define-module (sly scene)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (sly camera)
  #:use-module (sly mesh)
  #:use-module (sly quaternion)
  #:use-module (sly signal)
  #:use-module (sly transform)
  #:use-module (sly transition)
  #:use-module (sly math vector)
  #:export (scene-node
            make-scene-node
            scene-node?
            scene-node-position scene-node-scale scene-node-rotation
            scene-node-uniforms scene-node-children
            update-scene-node draw-scene-node
            make-scene
            scene?
            scene-root
            update-scene draw-scene))

(define-record-type <scene-node>
  (%make-scene-node position scale rotation uniforms children)
  scene-node?
  (position scene-node-position)
  (scale scene-node-scale)
  (rotation scene-node-rotation)
  (uniforms scene-node-uniforms)
  (children scene-node-children))

(define* (make-scene-node #:optional #:key
                           (position (vector2 0 0))
                           (scale 1)
                           (rotation identity-quaternion)
                           (uniforms '())
                           (children '())
                           #:allow-other-keys)
  (%make-scene-node position scale rotation uniforms children))

(define-syntax-rule (scene-node (field val) ...)
  (apply make-scene-node
         (append (list (symbol->keyword 'field) val) ...)))

(define (interpolate current prev alpha)
  (if (or (not prev)
          (equal? current prev))
      current
      (vector-interpolate prev current alpha)))

(define* (draw-scene-node node alpha transform #:optional (uniforms '()))
  (signal-let ((node node))
    (if (mesh? node)
        (draw-mesh node `(("mvp" ,transform)
                          ,@uniforms))
        (signal-let ((children (scene-node-children node))
                     (position (scene-node-position node))
                     (%scale (scene-node-scale node))
                     (rotation (scene-node-rotation node)))
          (let ((node-transform (transform*
                                 (translate position)
                                 (scale %scale)
                                 (quaternion->transform rotation))))
            (for-each (cut draw-scene-node <> alpha
                           (transform* transform node-transform)
                           (scene-node-uniforms node))
                      children))))))

;;;
;;; Scene
;;;

(define-record-type <scene>
  (make-scene root cameras)
  scene?
  (root scene-root)
  (cameras scene-cameras))

(define (update-scene scene)
  "Update the nodes within SCENE."
  (update-scene-node (scene-root scene)))

(define (draw-scene scene alpha)
  "Draw SCENE from the perspective of CAMERA with interpolation factor
ALPHA."
  (for-each (lambda (camera)
              (call-with-camera camera
                (lambda (projection location)
                  (draw-scene-node (scene-root scene)
                                   alpha
                                   (transform* projection location)))))
            (scene-cameras scene)))
