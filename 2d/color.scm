;;; guile-2d
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
;; Color.
;;
;;; Code:

(define-module (2d color)
  #:use-module (gl)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (2d math)
  #:export (<color>
            make-color
            color?
            color-r
            color-g
            color-b
            color-a
            rgba
            rgb
            color*
            color+
            color-
            color-inverse
            white
            black
            red
            green
            blue
            yellow
            magenta
            cyan
            tango-light-butter
            tango-butter
            tango-dark-butter
            tango-light-orange
            tango-orange
            tango-dark-orange
            tango-light-chocolate
            tango-chocolate
            tango-dark-chocolate
            tango-light-chameleon
            tango-chameleon
            tango-dark-chameleon
            tango-light-sky-blue
            tango-sky-blue
            tango-dark-sky-blue
            tango-light-plum
            tango-plum
            tango-dark-plum
            tango-light-scarlet-red
            tango-scarlet-red
            tango-dark-scarlet-red
            tango-aluminium-1
            tango-aluminium-2
            tango-aluminium-3
            tango-aluminium-4
            tango-aluminium-5
            tango-aluminium-6))

(define-record-type <color>
  (%make-color r g b a)
  color?
  (r color-r)
  (g color-g)
  (b color-b)
  (a color-a))

(define (make-color r g b a)
  "Return a newly allocated color with the given RGBA channel values.
Each channel is clamped to the range [0, 1]."
  (%make-color (clamp 0 1 r)
               (clamp 0 1 g)
               (clamp 0 1 b)
               (clamp 0 1 a)))

(define (color-component color-code offset)
  "Return the value of an 8-bit color channel in the range [0,1] for
the integer COLOR-CODE, given an OFFSET in bits."
  (let ((mask (ash #xff offset)))
    (/ (ash (logand mask color-code)
            (- offset))
       255.0)))

(define (rgba color-code)
  "Translate an RGBA format string COLOR-CODE into a color object.
For example: #xffffffff will return a color with RGBA values 1, 1, 1,
1."
  (%make-color (color-component color-code 24)
               (color-component color-code 16)
               (color-component color-code 8)
               (color-component color-code 0)))

(define (rgb color-code)
  "Translate an RGB format string COLOR-CODE into a color object.
For example: #xffffff will return a color with RGBA values 1, 1, 1,
1."
  (%make-color (color-component color-code 16)
               (color-component color-code 8)
               (color-component color-code 0)
               1))

(define (color* a b)
  "Multiply the RGBA channels of colors A and B."
  (make-color (* (color-r a)
                 (color-r b))
              (* (color-g a)
                 (color-g b))
              (* (color-b a)
                 (color-b b))
              (* (color-a a)
                 (color-a b))))

(define (color+ a b)
  "Add the RGBA channels of colors A and B."
  (make-color (+ (color-r a)
                 (color-r b))
              (+ (color-g a)
                 (color-g b))
              (+ (color-b a)
                 (color-b b))
              (+ (color-a a)
                 (color-a b))))

(define (color- a b)
  "Subtract the RGBA channels of colors A and B."
  (make-color (- (color-r a)
                 (color-r b))
              (- (color-g a)
                 (color-g b))
              (- (color-b a)
                 (color-b b))
              (- (color-a a)
                 (color-a b))))

(define (color-inverse color)
  "Create a new color that is the inverse of COLOR.  The alpha channel
is left unchanged."
  (make-color (- 1 (color-r color))
              (- 1 (color-g color))
              (- 1 (color-b color))
              (color-a color)))

;;;
;;; Pre-defined Colors
;;;

;; Basic
(define white (rgb #xffffff))
(define black (rgb #x000000))
(define red (rgb #xff0000))
(define green (rgb #x00ff00))
(define blue (rgb #x0000ff))
(define yellow (rgb #xffff00))
(define magenta (rgb #xff00ff))
(define cyan (rgb #x00ffff))

;; Tango color pallete
;; http://tango.freedesktop.org
(define tango-light-butter (rgb #xfce94f))
(define tango-butter (rgb #xedd400))
(define tango-dark-butter (rgb #xc4a000))
(define tango-light-orange (rgb #xfcaf3e))
(define tango-orange (rgb #xf57900))
(define tango-dark-orange (rgb #xce5c00))
(define tango-light-chocolate (rgb #xe9b96e))
(define tango-chocolate (rgb #xc17d11))
(define tango-dark-chocolate (rgb #x8f5902))
(define tango-light-chameleon (rgb #x8ae234))
(define tango-chameleon (rgb #x73d216))
(define tango-dark-chameleon (rgb #x4e9a06))
(define tango-light-sky-blue (rgb #x729fcf))
(define tango-sky-blue (rgb #x3465a4))
(define tango-dark-sky-blue (rgb #x204a87))
(define tango-light-plum (rgb #xad7fa8))
(define tango-plum (rgb #x75507b))
(define tango-dark-plum (rgb #x5c3566))
(define tango-light-scarlet-red (rgb #xef2929))
(define tango-scarlet-red (rgb #xcc0000))
(define tango-dark-scarlet-red (rgb #xa40000))
(define tango-aluminium-1 (rgb #xeeeeec))
(define tango-aluminium-2 (rgb #xd3d7cf))
(define tango-aluminium-3 (rgb #xbabdb6))
(define tango-aluminium-4 (rgb #x888a85))
(define tango-aluminium-5 (rgb #x555753))
(define tango-aluminium-6 (rgb #x2e3436))
