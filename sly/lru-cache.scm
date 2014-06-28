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
;; Least recently used cache implementation.
;;
;;; Code:

(define-module (sly lru-cache)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9))

(define (memoize/lru proc max-size)
  (let ((cache (make-hash-table))
        (keys '())
        (size 0))
    (lambda args
      (let ((results (hash-ref cache args)))
        (if results
            (apply values results)
            (let ((results (call-with-values (lambda ()
                                               (apply proc args))
                             list)))
              (hash-set! cache args results)
              (set! size (1+ size))
              (apply values results)))))))

(define (memoize proc)
  "Return a memoizing version of PROC."
  (let ((cache (make-hash-table)))
    (lambda args
      (let ((results (hash-ref cache args)))
        (if results
            (apply values results)
            (let ((results (call-with-values (lambda ()
                                               (apply proc args))
                             list)))
              (hash-set! cache args results)
              (apply values results)))))))
