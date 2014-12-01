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

(define-module (sly math tween)
  #:use-module (sly math)
  #:use-module (sly utils)
  #:export (ease-loop ease-reflect ease-linear
            ease-in-sine ease-out-sine ease-in-out-sine
            ease-in-quad ease-out-quad ease-in-out-quad
            tween))

;;;
;;; Easings
;;;

(define (ease-loop alpha)
  (modulo* alpha 1))

(define (ease-reflect alpha)
  (let ((cycle (modulo* alpha 2)))
    (if (< cycle 1) cycle (- 2 cycle))))

(define (ease-linear alpha)
  alpha)

(define (ease-in-sine alpha)
  (+ (* (- alpha) (cos (* alpha pi/2))) alpha))

(define (ease-out-sine alpha)
  (* alpha (sin (* alpha pi/2))))

(define (ease-in-out-sine alpha)
  (* (/ alpha -2)
     (1- (cos (* alpha pi)))))

(define (ease-in-quad alpha)
  (expt alpha 3))

(define (ease-out-quad alpha)
  (* (- alpha) alpha (- alpha 2)))

(define (ease-in-out-quad alpha)
  (let ((beta (* alpha 2)))
    (if (< beta 1)
        (* (/ alpha 2) beta beta)
        (* (/ alpha -2) (1- (* (1- beta) (- beta 3)))))))

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
;;; Tween
;;;

(define* (tween interpolator ease start end duration)
  "Return a procedure that interpolates from START to END in DURATION
ticks.  The value returned for a given time is determined by applying
EASE with the time ratio to acquire an 'alpha' value, and then
applying INTERPOLATOR with START, END, and alpha.  Alpha is a rational
number in the range [0, 1]."
  (define interpolate
    (memoize
     (lambda (alpha)
       (interpolator start end alpha))))

  (lambda (time)
    (interpolate (clamp 0 1 (ease (/ time duration))))))
