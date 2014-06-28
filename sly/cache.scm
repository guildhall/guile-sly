;;; Sly
;;; Copyright (C) 2014 David Thompson <dthompson2@worcester.edu>
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
;; Asset cache.
;;
;;; Code:

(define-module (sly cache)
  #:use-module (srfi srfi-9)
  #:export (make-cache
            cache?
            cache-ref
            cache-set!
            cached?
            cached))

(define-record-type <cache>
  (%make-cache table max-size)
  cache?
  (table cache-table)
  (max-size cache-max-size))

(define (make-cache max-size)
  (%make-cache (make-weak-key-hash-table) max-size))

(define (cache-miss! cache proc args)
  (let ((results (call-with-values (lambda ()
                                     (apply proc args))
                   list)))
    (hash-set! (cache-table cache) (cons proc args) results)
    results))

(define (cache-ref cache proc args)
  (hash-ref (cache-table cache) (cons proc args)))

(define (cached cache proc . args)
  (apply values (or (cache-ref cache proc args)
                    (cache-miss! cache proc args))))

(define (cache-clear! cache)
  (hash-clear! (cache-table cache)))

(define-syntax-rule (define-cached (name arg ...)
                      default-cache docstring body ...)
  (define (name arg ...)
    docstring
    (cached default-cache (lambda (arg ...) body ...) arg ...)))

(define font-cache (make-cache 100))

(define-cached (load-font filename) font-cache
  "Hello."
  (display "load!\n")
  (format #f "loaded font ~a" filename))

(define texture-cache (make-cache 100))

(define (%load-texture filename)
  (display "load!\n")
  (format #f "~a loaded" filename))

(define (load-texture filename)
  (cached texture-cache %load-texture filename))

(load-texture "foo.jpg")

(cached texture-cache "bar" (load-texture "foo"))

(define-cached (load-texture filename)
  ())

(load-cache 'image "images/p1_front.png")
(load-asset cache 'font "fonts/sans.ttf" 16)

(load-asset 'clear)
