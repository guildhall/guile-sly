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
;; Hierarchy of renderable objects using a directed acyclic graph
;; structure.
;;
;;; Code:

(define-module (sly scene)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (sly signal)
  #:use-module (sly transform)
  #:use-module (sly math vector)
  #:use-module (sly render utils)
  #:use-module (sly render camera)
  #:use-module (sly render renderer)
  #:export (scene-node make-scene-node
            scene-node?
            scene-node-object scene-node-transform
            scene-node-visible? scene-node-children
            draw-scene))

(define-record-type <scene-node>
  (%make-scene-node object transform visible? children)
  scene-node?
  (object scene-node-object)
  (transform scene-node-transform)
  (visible? scene-node-visible?)
  (children scene-node-children))

(define* (make-scene-node #:optional object #:key (transform identity-transform)
                          (visible? #t) (children '()))
  "Create a new scene node containing OBJECT, a renderable object that
responds to the 'draw' method.  The node has a local transformation
matrix TRANSFORM, and a list of CHILDREN. The VISIBLE? flag etermines
whether to draw the node and all of its children or not."
  (%make-scene-node object transform visible? children))

(define scene-node make-scene-node)

(define (flatten lst)
  "Return a list that recursively concatenates all sub-lists of LIST."
  (fold-right
   (match-lambda*
    (((sub-list ...) memo)
     (append (flatten sub-list) memo))
    ((elem memo)
     (cons elem memo)))
   '() lst))

(define (scene->renderer node camera)
  "Traverse the scene graph defined by NODE and its children, as seen
by CAMERA, and return a list of the render operations needed to
display the scene."
  (define (iter node parent-transform)
    (signal-let ((node node))
      (if (scene-node-visible? node)
          (let ((transform (transform* parent-transform
                                       (scene-node-transform node)))
                (object (scene-node-object node)))
            (cons (if object
                      (draw (scene-node-object node) transform)
                      '())
                  (map (cut iter <> transform)
                       (scene-node-children node))))
          '())))
  (let ((view (transform* (camera-projection camera)
                          (camera-location camera))))
    (make-renderer (flatten (iter node view)))))

(define (draw-scene node camera)
  "Draw the scene defined by NODE, as seen by CAMERA."
  (apply-viewport (camera-viewport camera))
  (render (scene->renderer node camera)))
