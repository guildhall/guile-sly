;;; Sly
;;; Copyright (C) 2014 David Thompson <davet@gnu.org>
;;;
;;; Sly is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Sly is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Interpolate values over time with easing.  Useful for animation.
;;
;;; Code:

(define-module (sly transition)
  #:use-module (ice-9 match)
  #:use-module (sly agenda)
  #:use-module (sly color)
  #:use-module (sly coroutine)
  #:use-module (sly math)
  #:use-module (sly quaternion)
  #:use-module (sly signal)
  #:use-module (sly math vector)
  #:export (ease-linear
            ease-in-sine ease-out-sine ease-in-out-sine
            ease-in-quad ease-out-quad ease-in-out-quad
            interpolator
            number-interpolate vector-interpolate color-interpolate
            transition))

;;;
;;; Easings
;;;

(define (ease-linear t d)
  (/ t d))

(define (ease-in-sine t d)
  (let ((delta (/ t d)))
    (+ (* (- delta) (cos (* delta pi/2))) delta)))

(define (ease-out-sine t d)
  (let ((delta (/ t d)))
    (* delta (sin (* delta pi/2)))))

(define (ease-in-out-sine t d)
  (* (/ t d -2)
     (1- (cos (/ (* t pi) d)))))

(define (ease-in-quad t d)
  (expt (/ t d) 3))

(define (ease-out-quad t d)
  (let ((delta (/ t d)))
    (* (- delta) delta (- delta 2))))

(define (ease-in-out-quad t d)
  (let ((delta (/ t d))
        (t (/ t (/ d 2))))
    (if (< t 1)
        (* (/ delta 2) t t)
        (* (/ delta -2)
           (1- (* (1- t) (- t 3)))))))

;; TODO: See
;; <http://gsgd.co.uk/sandbox/jquery/easing/jquery.easing.1.3.js> for
;; implementation details.
;;
;; ease-in-cubic
;; ease-out-cubic
;; ease-in-out-cubic
;; ease-in-quart
;; ease-out-quart
;; ease-in-out-quart
;; ease-in-quint
;; ease-out-quint
;; ease-in-out-quint
;; ease-in-expo
;; ease-out-expo
;; ease-in-out-expo
;; ease-in-circ
;; ease-out-circ
;; ease-in-out-circ
;; ease-in-back
;; ease-out-back
;; ease-in-out-back
;; ease-in-elastic
;; ease-out-elastic
;; ease-in-out-elastic
;; ease-in-bounce
;; ease-out-bounce
;; ease-in-out-bounce

;;;
;;; Transitions
;;;

(define guess-interpolator
  (match-lambda*
   (((? number? _) (? number? _))
    lerp)
   ((or ((? vector2? _) (? vector2? _))
        ((? vector3? _) (? vector3? _))
        ((? vector4? _) (? vector4? _)))
    vlerp)
   (((? color? _) (? color? _))
    color-lerp)
   (((? quaternion? _) (? quaternion? _))
    quaternion-slerp)
   ((a b)
    (error "Failed to guess interpolator: " a b))))

(define* (transition start end duration
                     #:optional #:key
                     (interpolator #f)
                     (ease ease-linear)
                     (step 1))
  "Return a signal that transitions from START to END in DURATION
ticks. The INTERPOLATOR procedure is used to compute intermediate
values.  When no interpolator is specified, it is inferred.
Interpolation procedures can be inferred for numbers, vectors, and
colors.  For other values, an error is thrown unless INTERPOLATOR is
passed explicitly.  EASE specifies the easing procedure to apply to
the transition.  Linear easing is used by default.  STEP specifies the
number of ticks between each interpolation sample.  By default, a step
of 1 is used for the smoothest, but most computationally expensive
transition."
  (let ((interpolator (or interpolator
                          (guess-interpolator start end))))
    (define (value-at t)
      (interpolator start end (ease t duration)))

    (signal-generator
     (yield start)
     (let lp ((t 0))
       (if (< t duration)
           (begin
             (wait (min step (- duration t)))
             (yield (value-at t))
             (lp (+ t step)))
           (yield end))))))
