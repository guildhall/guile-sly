;;; guile-2d
;;; Copyright (C) 2013, 2014 David Thompson <dthompson2@worcester.edu>
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

(define-module (2d signal)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (2d agenda)
  #:export (signal?
            make-signal
            define-signal
            hook->signal
            signal-ref
            signal-ref-maybe
            signal-set!
            signal-proc
            signal-merge
            signal-combine
            signal-map
            signal-fold
            signal-filter
            signal-reject
            signal-constant
            signal-count
            signal-do
            signal-sample
            signal-delay
            signal-throttle))

;;;
;;; Signals
;;;

;; Signals are time-varying values. For example, a signal could
;; represent the mouse position at the current point in time. The
;; signals API provides an abstraction over regular event-based
;; programming. State mutation is hidden away and a functional,
;; declarative interface is exposed.
(define-record-type <signal>
  (%%make-signal value proc inputs outputs)
  %signal?
  (value %signal-ref %%signal-set!)
  (proc signal-proc)
  (inputs signal-inputs)
  (outputs signal-outputs))

(define-record-type <signal-box>
  (make-signal-box signal)
  signal-box?
  (signal signal-unbox signal-box-set!))

;; Alternate spelling of signal-box? for the public API.
(define signal? signal-box?)

(define (%make-signal init proc inputs)
  "Create a new signal with initial value INIT."
  (let ((signal (%%make-signal init proc inputs (make-weak-key-hash-table))))
    (for-each (cut signal-connect! signal <>) inputs)
    signal))

(define (make-signal init)
  "Return a signal box with initial value INIT."
  (make-signal-box (%make-signal init #f '())))

(define (make-boxed-signal init proc inputs)
  "Return a signal box containing a signal with value INIT, updating
procedure PROC, and a list of INPUTS."
  (make-signal-box (%make-signal init proc inputs)))

(define (signal-connect! signal-out signal-box-in)
  "Attach SIGNAL-OUT to SIGNAL-BOX-IN.  When the signal within
SIGNAL-BOX-IN changes, the value will be propagated to SIGNAL-OUT."
  (hashq-set! (signal-outputs (signal-unbox signal-box-in)) signal-out #f))

(define (signal-ref signal-box)
  "Return the current value of the signal contained within
SIGNAL-BOX."
  (%signal-ref (signal-unbox signal-box)))

(define (signal-ref-maybe object)
  "Retrieves the signal value from OBJECT if it is a signal and or
simply returns OBJECT otherwise."
  (if (signal-box? object)
      (signal-ref object)
      object))

(define (signal-propagate! signal)
  "Notify all output signals about the current value of SIGNAL."
  (hash-for-each (lambda (output unused)
                   ((signal-proc output) output (%signal-ref signal)))
                 (signal-outputs signal)))

(define (%signal-set! signal value)
  "Change the current value of SIGNAL to VALUE and propagate VALUE to
all output signals."
  (%%signal-set! signal value)
  (signal-propagate! signal)
  *unspecified*)

(define (signal-set! signal-box value)
  "Change the current value contained within SIGNAL-BOX to VALUE."
  (%signal-set! (signal-unbox signal-box) value))

(define (splice-signals! box-to box-from)
  "Replace the contents of BOX-TO with the contents of BOX-FROM and
transfer all output signals."
  (when (signal-box? box-to)
    (let ((outputs (signal-outputs (signal-unbox box-to))))
      (hash-for-each (lambda (signal unused)
                       (signal-connect! signal box-from))
                     outputs))
    (signal-box-set! box-to (signal-unbox box-from))))

(define-syntax define-signal
  (lambda (x)
    "Define a variable that contains a signal, with the added bonus
that if the variable already contains a signal then its outputs will
be spliced into the new signal."
    (syntax-case x ()
      ((_ name (signal ...))
       (defined? (syntax->datum #'name))
       #'(begin
           (splice-signals! name (signal ...))
           (signal-propagate! (signal-unbox name))))
      ((_ name (signal ...))
       #'(define name (signal ...)))
      ((_ name value)
       #'(define name value)))))

;;;
;;; Higher Order Signals
;;;

(define (hook->signal hook init proc)
  "Return a new signal whose initial value is INIT and has future
values calculated by applying PROC to the arguments sent when HOOK is
run."
  (let ((signal (make-signal init)))
    (add-hook! hook
               (lambda args
                 (signal-set! signal (apply proc args))))
    signal))

(define (signal-merge signal1 signal2 . rest)
  "Create a new signal whose value is the that of the most recently
changed signal in SIGNALs.  The initial value is that of the first
signal in SIGNALS."
  (let ((inputs (append (list signal1 signal2) rest)))
    (make-boxed-signal (signal-ref (car inputs))
                       (lambda (self value)
                         (%signal-set! self value))
                       inputs)))

(define (signal-combine . signals)
  "Create a new signal whose value is a list of the values stored in
the given signals."
  (define (current-value)
    (map signal-ref signals))
  (make-boxed-signal (current-value)
                     (lambda (self value)
                       (%signal-set! self (current-value)))
                     signals))

(define (signal-map proc signal . rest)
  "Create a new signal that applies PROC to the values stored in one
or more SIGNALS."
  (let ((inputs (cons signal rest)))
    (define (current-value)
      (apply proc (map signal-ref inputs)))
    (make-boxed-signal (current-value)
                       (lambda (self value)
                         (%signal-set! self (current-value)))
                       inputs)))

(define (signal-fold proc init signal . rest)
  "Create a new signal that applies PROC to the values stored in
SIGNAL. PROC is applied with the current value of SIGNAL and the
previously computed value, or INIT for the first call."
  (let ((inputs (cons signal rest)))
    (make-boxed-signal init
                       (let ((previous init))
                         (lambda (self value)
                           (let ((x (apply proc
                                           (append (map signal-ref inputs)
                                                   (list previous)))))
                             (set! previous x)
                             (%signal-set! self x))))
                       inputs)))

(define (signal-filter predicate default signal)
  "Create a new signal that keeps an incoming value from SIGNAL when
it satifies the procedure PREDICATE.  The value of the signal is
DEFAULT when the predicate is never satisfied."
  (make-boxed-signal (if (predicate (signal-ref signal))
                         (signal-ref signal)
                         default)
                     (lambda (self value)
                       (when (predicate value)
                         (%signal-set! self value)))
                     (list signal)))

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

(define (signal-do proc signal)
  "Create a new signal that applies PROC when a new values is received
from SIGNAL.  The value of the new signal will always be the value of
SIGNAL.  This signal is a convenient way to sneak a procedure that has
a side-effect into a signal chain."
  (signal-map (lambda (x) (proc x) x) signal))

(define (signal-sample agenda delay signal)
  "Create a new signal that emits the value contained within SIGNAL
every DELAY ticks of AGENDA."
  (let ((sampler (%make-signal (signal-ref signal) #f '())))
    (define (tick)
      (%signal-set! sampler (signal-ref signal)))
    (schedule-interval agenda tick delay)
    (make-signal-box sampler)))

(define (signal-delay agenda delay signal)
  "Create a new signal that delays propagation of SIGNAL by DELAY
ticks of AGENDA."
  (make-boxed-signal (signal-ref signal)
                     (lambda (self value)
                       (schedule agenda
                                 (lambda ()
                                   (%signal-set! self value))
                                 delay))
                     (list signal)))

(define (signal-throttle agenda delay signal)
  "Return a new signal that propagates SIGNAL at most once every DELAY
ticks of AGENDA."
  (make-boxed-signal (signal-ref signal)
                     (let ((last-time (agenda-time agenda)))
                       (lambda (self value)
                         (when (>= (- (agenda-time agenda) last-time) delay)
                           (%signal-set! self value)
                           (set! last-time (agenda-time agenda)))))
                     (list signal)))
