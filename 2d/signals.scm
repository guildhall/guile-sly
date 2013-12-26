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
;; Simple functional reactive programming API.
;;
;;; Code:

(define-module (2d signals)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (<signal>
            signal?
            root-signal?
            make-signal
            make-root-signal
            signal-ref
            signal-ref-maybe
            signal-receiver
            signal-inputs
            signal-outputs
            signal-connect!
            signal-disconnect!
            signal-clear!
            signal-set!
            signal-merge
            signal-combine
            signal-do
            signal-map
            signal-fold
            signal-filter
            signal-reject
            signal-constant
            signal-count))

;;;
;;; Signals
;;;

;; Signals are time-varying values. For example, a signal could
;; represent the mouse position at the current point in time. The
;; signals API provides an abstraction over regular event-based
;; programming. State mutation is hidden away and a functional,
;; declarative interface is exposed.
(define-record-type <signal>
  (%make-signal value receiver inputs outputs)
  signal?
  (value signal-ref %signal-set!)
  (receiver signal-receiver)
  (inputs signal-inputs %set-signal-inputs!)
  (outputs signal-outputs %set-signal-outputs!))

(define (make-signal init receiver input . inputs)
  "Create a new signal with initial value INIT, a procedure RECEIVER
to transform incoming signal values and one or more signals to connect
to."
  (let ((signal (%make-signal init receiver '() '())))
    (for-each (cut signal-connect! <> signal)
              (cons input inputs))
    signal))

(define (make-root-signal init)
  "Create a new root level signal with initial value INIT."
  (%make-signal init #f '() '()))

(define (root-signal? signal)
  "Returns true if a signal has no receiver procedure or false
otherwise."
  (not (signal-receiver signal)))

(define (signal-ref-maybe object)
  "Retrieves the signal value from OBJECT if it is a signal and or
simply returns OBJECT otherwise."
  (if (signal? object)
      (signal-ref object)
      object))

(define (signal-connect! signal-in signal-out)
  "Attach SIGNAL-OUT to SIGNAL-IN. When the value of SIGNAL-IN changes, the
value will be propagated to SIGNAL-OUT."
  (if (root-signal? signal-out)
      (error 'root-signal-error
             "Cannot connect to a root signal"
             signal-out)
      (let ((inputs  (signal-inputs  signal-out))
            (outputs (signal-outputs signal-in)))
        (%set-signal-inputs!  signal-out (cons signal-in  inputs))
        (%set-signal-outputs! signal-in  (cons signal-out outputs)))))

(define (signal-disconnect! signal-in signal-out)
  "Detach SIGNAL-OUT from SIGNAL-IN."
  (let ((inputs (signal-inputs signal-out))
        (outputs (signal-outputs signal-in)))
    (%set-signal-inputs!  signal-out (delete signal-in  inputs  eq?))
    (%set-signal-outputs! signal-in  (delete signal-out outputs eq?))))

(define (signal-clear-outputs! signal)
  "Disconnect all output signals from SIGNAL."
  (for-each (cut signal-disconnect! signal <>)
            (signal-outputs signal)))

(define (signal-clear-inputs! signal)
  "Disconnect all inputs signals from SIGNAL."
  (for-each (cut signal-disconnect! <> signal)
            (signal-inputs signal)))

(define (signal-receive to from)
  "Evaluate the receiver procedure for the signal TO with the signal
FROM."
  ((signal-receiver to) to from))

(define (signal-propagate signal)
  "Notify all connected signals about the current value of SIGNAL."
  (for-each (cut signal-receive <> signal)
            (signal-outputs signal)))

(define (signal-set! signal value)
  "Change the current value of SIGNAL to VALUE and propagate SIGNAL to
all connected signals."
  (%signal-set! signal value)
  (signal-propagate signal))

;;;
;;; Higher Order Signals
;;;

(define (signal-merge . signals)
  "Create a new signal whose value is the that of the most recently
changed signal in SIGNALs.  The initial value is that of the first
signal in SIGNALS."
  (apply make-signal
         (signal-ref (car signals))
         (lambda (merger from)
           (signal-set! merger (signal-ref from)))
         signals))

(define (signal-combine . signals)
  "Create a new signal whose value is a list of the values stored in
the list SIGNALS."
  (define (update)
    (map signal-ref signals))

  (apply make-signal
         (update)
         (lambda (combiner from)
           (signal-set! combiner (update)))
         signals))

(define (signal-do proc signal)
  "Create a new signal that applies PROC when a new values is received
from SIGNAL.  The value of the new signal will always be the value of
SIGNAL.  This signal is a convenient way to sneak a procedure that has
a side-effect into a signal chain."
  (make-signal (signal-ref signal)
               (lambda (do-signal from)
                 (let ((value (signal-ref signal)))
                   (proc value)
                   value))
               signal))

(define (signal-map proc signal . signals)
  "Create a new signal that applies PROC to the values stored in one
or more SIGNALS."
  (define (update)
    (apply proc (map signal-ref (cons signal signals))))

  (apply make-signal
         (update)
         (lambda (map-signal from)
           (signal-set! map-signal (update)))
         signal
         signals))

(define (signal-fold proc init signal)
  "Create a new signal that applies PROC to the values stored in
SIGNAL. PROC is applied with the current value of SIGNAL and the
previously computed value, or INIT for the first call."
  (make-signal init
               (let ((previous init))
                 (lambda (fold-signal from)
                   (let ((value (proc (signal-ref from) previous)))
                     (set! previous value)
                     (signal-set! fold-signal value))))
               signal))

(define (signal-filter predicate default signal)
  "Create a new signal that keeps an incoming value from SIGNAL when
it satifies the procedure PREDICATE.  The value of the signal is
DEFAULT when the predicate is never satisfied."
  (make-signal (if (predicate (signal-ref signal))
                   (signal-ref signal)
                   default)
               (lambda (filter from)
                 (when (predicate (signal-ref from))
                   (signal-set! filter (signal-ref from))))
               signal))

(define (signal-reject predicate default signal)
  "Create a new signal that does not keep an incoming value from
SIGNAL when it satisfies the procedure PREDICATE.  The value of the
signal is DEFAULT when the predicate is never satisfied."
  (signal-filter (lambda (x) (not (predicate x))) default signal))

(define (signal-constant constant signal)
  "Create a new signal whose value is always CONSTANT regardless of
what the value received from SIGNAL."
  (signal-map (lambda (value) constant) signal))

(define (signal-count signal)
  "Create a new signal that increments a counter every time a new
value from SIGNAL is received."
  (signal-fold + 0 (signal-constant 1 signal)))
