;;; Sly
;;; Copyright (C) 2013, 2014 David Thompson <dthompson2@worcester.edu>
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
;; Tilesets encapsulate a group of uniformly sized texture regions
;; that come from a single texture.
;;
;;; Code:

(define-module (sly render tileset)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-42)
  #:use-module (sly render texture)
  #:export (<tileset>
            make-tileset
            load-tileset
            tileset?
            tileset-texture
            tileset-tiles
            tileset-width
            tileset-height
            tileset-margin
            tileset-spacing
            tileset-rows
            tileset-columns
            tileset-ref
            tileset-invert-index))

(define-record-type <tileset>
  (%make-tileset texture tiles width height margin spacing rows columns)
  tileset?
  (texture tileset-texture)
  (tiles tileset-tiles)
  (width tileset-width)
  (height tileset-height)
  (margin tileset-margin)
  (spacing tileset-spacing)
  (rows tileset-rows)
  (columns tileset-columns))

(set-record-type-printer! <tileset>
  (lambda (tileset port)
    (format port
            "#<tileset texture: ~a width: ~d height ~d margin: ~d spacing: ~d rows: ~d columns: ~d>"
            (tileset-texture tileset)
            (tileset-width tileset)
            (tileset-height tileset)
            (tileset-margin tileset)
            (tileset-spacing tileset)
            (tileset-rows tileset)
            (tileset-columns tileset))))

(define (split-texture texture width height margin spacing)
  "Split TEXTURE into a vector of texture regions of WIDTH x HEIGHT
size. SPACING refers to the number of pixels separating each
tile. MARGIN refers to the number of pixels on the top and left of
TEXTURE before the first tile begins."
  (define (build-tile tx ty)
    (let* ((x (+ (* tx (+ width spacing)) margin))
           (y (+ (* ty (+ height spacing)) margin)))
      (make-texture-region texture x y width height)))

  (let* ((tw (texture-width texture))
         (th (texture-height texture))
         (rows (/ (- th margin) (+ height spacing)))
         (columns (/ (- tw margin) (+ width spacing))))
    (values (vector-ec (: y rows) (: x columns) (build-tile x y))
            rows columns)))

(define* (make-tileset texture width height
                       #:optional #:key (margin 0) (spacing 0))
  "Return a new tileset that is built by splitting TEXTURE into
tiles."
  (let-values (((tiles rows columns)
                (split-texture texture width height margin spacing)))
    (%make-tileset texture tiles width height margin spacing rows columns)))

(define* (load-tileset filename width height
                       #:optional #:key (margin 0) (spacing 0))
  "Return a new tileset that is built by loading the texture at
FILENAME and splitting the texture into tiles."
  (let*-values (((texture) (load-texture filename))
                ((tiles rows columns)
                 (split-texture texture width height margin spacing)))
    (%make-tileset texture tiles width height margin spacing rows columns)))

(define (tileset-ref tileset i)
  "Return the tile texture of TILESET at index I."
  (vector-ref (tileset-tiles tileset) i))

(define (tileset-invert-index tileset index)
  "Convert INDEX, whose origin is the top-left corner of TILESET, to
an index whose origin is the bottom-left corner."
  (let* ((w (tileset-columns tileset))
         (h (tileset-rows tileset))
         (x (modulo index w))
         (y (- h 1 (floor (/ index w)))))
    (+ (* y w) x)))
