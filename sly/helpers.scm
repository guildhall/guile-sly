;;; Sly
;;; Copyright (C) 2013, 2014 David Thompson <dthompson2@worcester.edu>
;;; Copyright (C) 2014 Ludovic Courtès <ludo@gnu.org>
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
;; Miscellaneous helper procedures.
;;
;;; Code:

(define-module (sly helpers)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (sly agenda)
  #:export (any-equal?
            logand?
            define-guardian
            memoize
            forever))

(define (any-equal? elem . args)
  "Return #t if ELEM equals any of the elements in the list ARGS."
  (any (lambda (e) (equal? elem e)) args))

(define (logand? . args)
  "Return #t if the result of a bitwise AND of the integers in list
ARGS is non-zero."
  (not (zero? (apply logand args))))

(define-syntax-rule (define-guardian name reaper)
  "Define a new guardian called NAME and call REAPER when an object
within the guardian is GC'd.  Reaping is ensured to happen from the
same thread that is running the game loop."
  (begin
    (define name (make-guardian))
    (schedule-each
     (lambda ()
       (let reap ((obj (name)))
         (when obj
           (reaper obj)
           (reap (name))))))))

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

(define-syntax-rule (forever body ...)
  (while #t body ...))
