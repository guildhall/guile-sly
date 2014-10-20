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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (sly camera)
  #:use-module (sly mesh)
  #:use-module (sly quaternion)
  #:use-module (sly signal)
  #:use-module (sly texture)
  #:use-module (sly transform)
  #:use-module (sly transition)
  #:use-module (sly math vector)
  #:export (scene-node
            make-scene-node
            scene-node?
            scene-node-transform scene-node-texture
            scene-node-uniforms scene-node-children
            update-scene-node draw-scene-node
            make-scene
            scene?
            scene-root
            update-scene draw-scene))

(define-record-type <scene-node>
  (%make-scene-node transform texture uniforms children)
  scene-node?
  (transform scene-node-transform)
  (texture scene-node-texture)
  (uniforms scene-node-uniforms)
  (children scene-node-children))

(define* (make-scene-node #:optional #:key
                          (transform identity-transform)
                          (texture #f)
                          (uniforms '())
                          (children '())
                          #:allow-other-keys)
  (match children
    ((or (children ...)
         (= list children))
     (%make-scene-node transform texture uniforms children))))

(define-syntax-rule (scene-node (field val) ...)
  (apply make-scene-node
         (append (list (symbol->keyword 'field) val) ...)))

(define-syntax-rule (with-texture-maybe texture body ...)
  (if (texture? texture)
      (with-texture texture body ...)
      (begin body ...)))

(define* (draw-scene-node node alpha transform #:optional (uniforms '()))
  (signal-let ((node node))
    (if (mesh? node)
        (draw-mesh node `(("mvp" ,transform)
                          ,@uniforms))
        (signal-let ((children (scene-node-children node))
                     (local-transform (scene-node-transform node))
                     (texture (scene-node-texture node)))
          (with-texture-maybe texture
            (let ((transform (transform* transform local-transform))
                  ;; FIXME: properly merge uniform alists together.
                  (uniforms (append uniforms (scene-node-uniforms node))))
              (for-each (cut draw-scene-node <> alpha transform uniforms)
                        children)))))))

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
