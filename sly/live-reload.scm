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
  #:export (live-reload))

(define* (live-reload proc #:optional (polling-interval 120))
  "Return a new procedure that re-applies PROC whenever the associated
file is modified.  The new procedure returns a signal that contains
the return value of PROC.  The first argument to PROC must be a
file name string.

A simple polling method is used to test for updates.  Files are polled
every POLLING-INTERVAL ticks (120 by default)."
  (lambda (file-name . args)
    (define (load-asset)
      (apply proc file-name args))

    (define (current-mtime)
      (let ((info (stat file-name)))
        (max (stat:mtime info) (stat:ctime info))))

    (let ((asset (make-signal (load-asset))))
      (coroutine
       (let loop ((last-mtime (current-mtime)))
         (wait polling-interval)
         (let ((mtime (if (file-exists? file-name)
                          (current-mtime)
                          last-mtime)))
           (when (> mtime last-mtime)
             (signal-set! asset (load-asset)))
           (loop mtime))))
      asset)))
