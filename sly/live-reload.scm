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
;; Live asset reloading.
;;
;;; Code:

(define-module (sly live-reload)
  #:use-module (srfi srfi-1)
  #:use-module (sly agenda)
  #:use-module (sly coroutine)
  #:use-module (sly signal)
  #:export (live-reload-interval
            live-reload))

(define live-reload-interval 120)

(define (live-reload proc)
  "Return a new procedure that re-applies PROC whenever the associated
file is modified.  The new procedure returns a signal that contains
the return value of PROC.  The first argument to PROC must be a
filename string."
  (lambda (filename . args)
    (define (load-asset)
      (apply proc filename args))

    (define (current-mtime)
      (stat:mtime (stat filename)))

    (let ((asset (make-signal (load-asset))))
     (coroutine
      (let loop ((last-mtime (current-mtime)))
        (wait live-reload-interval)
        (let ((mtime (current-mtime)))
          (when (> mtime last-mtime)
            (signal-set! asset (load-asset)))
          (loop mtime))))
     asset)))
