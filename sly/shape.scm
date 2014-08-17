;;; Sly
;;; Copyright (C) 2014 David Thompson <davet@gnu.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Primitive 2D/3D shapes.
;;
;;; Code:

(define-module (sly shape)
  #:use-module (sly math)
  #:use-module (sly mesh)
  #:use-module (sly shader)
  #:use-module (sly texture)
  #:export (make-cube))

(define* (make-cube size #:optional #:key (texture #f)
                    (shader (load-default-shader)))
  (let ((half-size (half size)))
    (make-mesh
     #:shader shader
     #:texture texture
     #:indices #(
                 ;; Front
                 0 3 2 0 2 1
                   ;; Back
                   4 6 7 4 5 6
                   ;; Top
                   8 11 10 8 10 9
                   ;; Bottom
                   12 14 15 12 13 14
                   ;; Left
                   16 19 18 16 18 17
                   ;; Right
                   20 22 23 20 21 22)
     #:data `(("position" ,(vector
                            ;; Front
                            (vector (- half-size) (- half-size) (- half-size))
                            (vector half-size (- half-size) (- half-size))
                            (vector half-size half-size (- half-size))
                            (vector (- half-size) half-size (- half-size))
                            ;; Back
                            (vector (- half-size) (- half-size) half-size)
                            (vector half-size (- half-size) half-size)
                            (vector half-size half-size half-size)
                            (vector (- half-size) half-size half-size)
                            ;; Top
                            (vector (- half-size) half-size (- half-size))
                            (vector half-size half-size (- half-size))
                            (vector half-size half-size half-size)
                            (vector (- half-size) half-size half-size)
                            ;; Bottom
                            (vector (- half-size) (- half-size) (- half-size))
                            (vector half-size (- half-size) (- half-size))
                            (vector half-size (- half-size) half-size)
                            (vector (- half-size) (- half-size) half-size)
                            ;; Left
                            (vector (- half-size) (- half-size) (- half-size))
                            (vector (- half-size) half-size (- half-size))
                            (vector (- half-size) half-size half-size)
                            (vector (- half-size) (- half-size) half-size)
                            ;; Right
                            (vector half-size (- half-size) (- half-size))
                            (vector half-size half-size (- half-size))
                            (vector half-size half-size half-size)
                            (vector half-size (- half-size) half-size)))
              ,@(if texture
                   (let ((s1 (texture-s1 texture))
                         (t1 (texture-t1 texture))
                         (s2 (texture-s2 texture))
                         (t2 (texture-t2 texture)))
                     `("texture"
                       ,(vector
                         ;; Front
                         (vector s1 t1)
                         (vector s2 t1)
                         (vector s2 t2)
                         (vector s1 t2)
                         ;; Back
                         (vector s1 t1)
                         (vector s2 t1)
                         (vector s2 t2)
                         (vector s1 t2)
                         ;; Top
                         (vector s1 t1)
                         (vector s2 t1)
                         (vector s2 t2)
                         (vector s1 t2)
                         ;; Bottom
                         (vector s1 t1)
                         (vector s2 t1)
                         (vector s2 t2)
                         (vector s1 t2)
                         ;; Left
                         (vector s1 t1)
                         (vector s2 t1)
                         (vector s2 t2)
                         (vector s1 t2)
                         ;; Right
                         (vector s1 t1)
                         (vector s2 t1)
                         (vector s2 t2)
                         (vector s1 t2))))
                   '())))))
