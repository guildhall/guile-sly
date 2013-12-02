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
  #:use-module (2d coroutine)
  #:export (<signal>
            signal?
            make-signal
            signal-ref
            signal-ref-maybe
            signal-transformer
            signal-filter
            signal-connectors
            signal-connect!
            signal-disconnect!
            signal-clear!
            signal-set!
            signal-identity
            signal-constant
            signal-lift
            signal-lift2
            signal-lift3
            signal-lift4
            signal-merge
            signal-combine
            signal-count
            signal-if
            signal-and
            signal-or))

;;;
;;; Signals
;;;

;; Signals are time-varying values. For example, a signal could
;; represent the mouse position at the current point in time. The
;; signals API provides an abstraction over regular event-based
;; programming. State mutation is hidden away and a functional,
;; declarative interface is exposed.
(define-record-type <signal>
  (%make-signal value transformer filter connectors)
  signal?
  (value signal-ref %signal-set!)
  (transformer signal-transformer)
  (filter signal-filter)
  (connectors signal-connectors %set-signal-connectors!))

(define (keep-all value old from)
  "Keep all values."
  #t)

(define* (make-signal transformer #:optional #:key
                      (init #f) (connectors '()) (filter keep-all))
  "Create a new signal with initial value INIT that uses the given
TRANSFORMER procedure to process incoming values from another
signal.  Additionally, the signal will be connected to all of the
signals in the list CONNECTORS."
  (let ((signal (%make-signal init transformer filter '())))
    (for-each (cut signal-connect! <> signal) connectors)
    signal))

(define (signal-ref-maybe object)
  "Dereferences OBJECT if it is a signal and returns OBJECT
otherwise."
  (if (signal? object)
      (signal-ref object)
      object))

(define (%signal-transform signal value from)
  "Call the transform procedure for SIGNAL with VALUE."
  ((signal-transformer signal) value (signal-ref signal) from))

(define (signal-connect! signal listener)
  "Attach LISTENER to SIGNAL. When the value of SIGNAL changes, the
value will be propagated to LISTENER."
  (%set-signal-connectors!
   signal
   (cons listener (signal-connectors signal)))
  (signal-set! listener (signal-ref signal)))

(define (signal-disconnect! signal listener)
  "Detach LISTENER from SIGNAL."
  (%set-signal-connectors!
   signal
   (delete listener (signal-connectors signal) eq?)))

(define (signal-clear! signal)
  "Detach all connectors from SIGNAL."
  (%set-signal-connectors! signal '()))

(codefine* (signal-set! signal value #:optional (from #f))
  "Set VALUE for SIGNAL from the connected signal FROM and
propagate VALUE to all connected signals. "
  (signal-set-and-propagate! signal value from))

(define (signal-set-and-propagate! signal value from)
  "Set VALUE for SIGNAL from the connected signal FROM and
propagate VALUE to all connected signals. "
  (let ((value (%signal-transform signal value from)))
    (%signal-set! signal value)
    (for-each (cut signal-receive! <> value signal)
              (signal-connectors signal))))

(define (signal-keep? signal value from)
  "Call the filter procedure for SIGNAL with VALUE."
  ((signal-filter signal) value (signal-ref signal) from))

(define (signal-receive! signal value from)
  "Receive VALUE for SIGNAL from the connected signal FROM.  VALUE
will be set if it passes through the filter."
  (when (signal-keep? signal value from)
    (signal-set-and-propagate! signal value from)))

;;;
;;; Primitive signals
;;;

(define* (signal-identity #:optional (init #f))
  "Create a new signal with initial value INIT whose transformer procedure
returns values unchanged."
  (make-signal (lambda (value prev from)
                 value)
               #:init init))

(define (signal-constant constant)
  "Create a new signal with a value CONSTANT that cannot be changed."
  (make-signal (lambda (value prev from)
                 constant)
               #:init constant))

;; TODO: Write a macro for generating lifts
(define (signal-lift transformer signal)
  "Create a new signal that lifts the procedure TRANSFORMER of arity 1
onto SIGNAL."
  (make-signal (lambda (value prev from)
                 (transformer value))
               #:connectors (list signal)))

(define (signal-lift2 transformer signal1 signal2)
  "Create a new signal that lifts the procedure TRANSFORMER of arity 2
onto SIGNAL1 and SIGNAL2."
  (make-signal (lambda (value prev from)
                 (transformer (signal-ref signal1)
                              (signal-ref signal2)))
               #:connectors (list signal1 signal2)))

(define (signal-lift3 transformer signal1 signal2 signal3)
  "Create a new signal that lifts the procedure TRANSFORMER of arity 3
onto SIGNAL1, SIGNAL2, and SIGNAL3."
  (make-signal (lambda (value prev from)
                 (transformer (signal-ref signal1)
                              (signal-ref signal2)
                              (signal-ref signal3)))
               #:connectors (list signal1 signal2 signal3)))

(define (signal-lift4 transformer signal1 signal2 signal3 signal4)
  "Create a new signal that lifts the procedure TRANSFORMER of arity 4
onto SIGNAL1, SIGNAL2, SIGNAL3, and SIGNAL4."
  (make-signal (lambda (value prev from)
                 (transformer (signal-ref signal1)
                              (signal-ref signal2)
                              (signal-ref signal3)
                              (signal-ref signal4)))
               #:connectors (list signal1 signal2 signal3 signal4)))

(define (signal-merge signal1 signal2)
  "Create a new signal that merges SIGNAL1 and SIGNAL2 into one. The
value of the new signal is the value of the most recently changed
parent signal."
  (make-signal (lambda (value prev from)
                 value)
               #:connectors (list signal1 signal2)))

(define (signal-combine . signals)
  "Create a new signal that combines the values of SIGNALS into a
list."
  (make-signal (lambda (value prev from)
                 (map signal-ref signals))
               #:connectors signals))

(define (signal-count signal)
  "Create a new signal that increments a counter every time the value
of SIGNAL changes."
  (make-signal (lambda (value prev from)
                 (1+ prev))
               #:connectors (list signal)))

(define (signal-if predicate consequent alternate)
  "Create a new signal that emits the value of the signal CONSEQUENT
when the value of the signal PREDICATE is true and the value of the
signal ALTERNATE otherwise."
  (make-signal (lambda (value prev from)
                 (if (signal-ref predicate)
                     (signal-ref consequent)
                     (signal-ref alternate)))
               #:connectors (list predicate
                                  consequent
                                  alternate)))

(define (signal-and . signals)
  "Create a new signal that performs a logical AND operation on the
values of SIGNALS."
  (make-signal (lambda (value prev from)
                 (let loop ((signals signals)
                            (prev #t))
                   (cond ((null? signals)
                          (signal-ref prev))
                         ((signal-ref (car signals))
                          (loop (cdr signals) (car signals)))
                         (else
                          #f))))
               #:connectors signals))

(define (signal-or . signals)
  "Create a new signal that performs a logicla OR operation the values
of SIGNALS."
  (make-signal (lambda (value prev from)
                 (let loop ((signals signals))
                   (cond ((null? signals)
                          #f)
                         ((signal-ref (car signals))
                          (signal-ref (car signals)))
                         (else
                          (loop (cdr signals))))))
               #:connectors signals))
