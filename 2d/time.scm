;;; guile-2d
;;; Copyright (C) 2013 David Thompson <dthompson2@worcester.edu>
;;;
;;; Guile-2d is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-2d is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Time-based signals.
;;
;;; Code:

(define-module (2d time)
  #:use-module (2d agenda)
  #:use-module (2d signals)
  #:export (time-every
            time-interval
            time-delay))

(define (time-interval ticks)
  "Create a new signal that sets its value to #t every TICKS agenda
updates."
  (let ((signal (make-signal #:init #t)))
    (agenda-schedule-interval
     (lambda ()
       (signal-set! signal #t)) ticks)
    signal))

(define (time-every)
  "Create a new signal that sets its value to #t every agenda update."
  (time-interval 1))

(define (time-delay ticks signal)
  (make-signal
   #:filter (lambda (value old from)
              (wait ticks)
              #t)
   #:connectors (list signal)))
