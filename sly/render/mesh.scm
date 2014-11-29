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
;; A mesh is a 2D/3D model comprised of a shader and vertex buffers.
;;
;;; Code:

(define-module (sly render mesh)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (gl)
  #:use-module (gl low-level)
  #:use-module (sly wrappers gl)
  #:use-module (sly render color)
  #:use-module (sly render shader)
  #:use-module (sly render texture)
  #:use-module (sly math vector)
  #:use-module (sly signal)
  #:use-module (sly math transform)
  #:use-module (sly render utils)
  #:use-module (sly render vertex-array)
  #:use-module (sly render model)
  #:export (make-mesh))

(define* (make-mesh #:optional #:key shader texture indices positions textures)
  (make-model #:shader shader #:texture texture
              #:mesh (make-vertex-array indices positions textures)))
