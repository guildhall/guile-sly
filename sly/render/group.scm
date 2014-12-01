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

(define-module (sly render group)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (sly math transform)
  #:use-module (sly render shader)
  #:use-module (sly render texture)
  #:use-module (sly render utils)
  #:use-module (sly render camera)
  #:use-module (sly render context)
  #:use-module (sly render model)
  #:export (make-group group
            group?
            group-transform group-visible? group-children
            draw-group
            group-move group-place group-show))

;;;
;;; Group
;;;

;; The composite object of the scene graph.  Groups have zero or more
;; children, which may be groups or models.
(define-record-type <group>
  (%make-group transform visible? children)
  group?
  (transform group-transform)
  (visible? group-visible?)
  (children group-children))

(define* (make-group children #:optional #:key (transform identity-transform)
                     (visible? #t))
  "Create a new group containing CHILDREN in which each child is
rendered relative to TRANSFORM.  The VISIBLE? flag determines whether
or not to render child nodes."
  (%make-group transform visible? children))

(define (%draw-group group parent-transform view context)
  (match group
    (($ <group> transform visible? children)
     (when visible?
       (with-temp-transform context world-transform
         (transform*! world-transform transform parent-transform)
         (for-each (lambda (child)
                     (let ((draw (match child
                                   ((? group? group) %draw-group)
                                   ((? model? model) draw-model))))
                       (draw child world-transform view context)))
                   children))))))

(define (set-transform-identity! t)
  (let ((matrix (transform-matrix t)))
    (array-set! matrix 1 0 0)
    (array-set! matrix 0 0 1)
    (array-set! matrix 0 0 2)
    (array-set! matrix 0 0 3)
    (array-set! matrix 0 1 0)
    (array-set! matrix 1 1 1)
    (array-set! matrix 0 1 2)
    (array-set! matrix 0 1 3)
    (array-set! matrix 0 2 0)
    (array-set! matrix 0 2 1)
    (array-set! matrix 1 2 2)
    (array-set! matrix 0 2 3)
    (array-set! matrix 0 3 0)
    (array-set! matrix 0 3 1)
    (array-set! matrix 0 3 2)
    (array-set! matrix 1 3 3)))

(define draw-group
  (let ((context (make-render-context)))
    (lambda* (group camera #:optional (context context))
      "Draw the scene defined by GROUP as viewed by CAMERA, with the given
render CONTEXT."
      (with-temp-transform context view
        (transform*! view
                     (camera-location camera)
                     (camera-projection camera))
        (with-temp-transform context base-transform
          (set-transform-identity! base-transform)
          (apply-viewport (camera-viewport camera))
          (%draw-group group base-transform view context))))))

;;;
;;; Utility Procedures
;;;

(define (group . children)
  "Create a new group containing the list of CHILDREN."
  (make-group children))

(define (group-move position group)
  "Create a new group in which the list of CHILDREN are translated by
the vector POSITION."
  (match group
    (($ <group> transform visible? children)
     (%make-group (transform* transform (translate position))
                  visible? children))))

(define (group-place transform group)
  "Create a new group in which the tranformation matrices of the
CHILDREN are multiplied by TRANSFORM."
  (match group
    (($ <group> original-transform visible? children)
     (%make-group (transform* original-transform transform)
                  visible? children))))

(define (group-show visible? group)
  "Create a new group in which the visibility of the list of
CHILDREN is determined by the VISIBLE? flag."
  (match group
    (($ <group> transform _ children)
     (%make-group transform visible? children))))
