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
;; Rendering utilities.
;;
;;; Code:

(define-module (sly render utils)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (gl low-level)
  #:use-module (gl)
  #:export (make-blend-mode blend-mode?
            blend-mode-source blend-mode-destination
            default-blend-mode
            apply-blend-mode))

;;;
;;; Blending
;;;

(define-record-type <blend-mode>
  (make-blend-mode source destination)
  blend-mode?
  (source blend-mode-source)
  (destination blend-mode-destination))

(define default-blend-mode
  (make-blend-mode 'src-alpha 'one-minus-src-alpha))

;; Translate symbols to OpenGL constants.
(define source-blend-function
  (match-lambda
   ('zero 0)
   ('one 1)
   ('dst-color 774)
   ('one-minus-dst-color 775)
   ('src-alpha-saturate 776)
   ('src-alpha 770)
   ('one-minus-src-alpha 771)
   ('dst-alpha 772)
   ('one-minus-dst-alpha 773)
   ('constant-color 32769)
   ('one-minus-constant-color 32770)
   ('constant-alpha 32771)
   ('one-minus-constant-alpha 32772)))

(define destination-blend-function
  (match-lambda
   ('zero 0)
   ('one 1)
   ('src-color 768)
   ('one-minus-src-color 769)
   ('src-alpha 770)
   ('one-minus-src-alpha 771)
   ('dst-alpha 772)
   ('one-minus-dst-alpha 773)
   ('constant-color 32769)
   ('one-minus-constant-color 32770)
   ('constant-alpha 32771)
   ('one-minus-constant-alpha 32772)))

(define (apply-blend-mode blend-mode)
  (if blend-mode
      (begin
        (gl-enable (enable-cap blend))
        (glBlendFunc (source-blend-function (blend-mode-source blend-mode))
                     (destination-blend-function
                      (blend-mode-destination blend-mode))))
      (gl-disable (enable-cap blend))))
