;;; Sly
;;; Copyright (C) 2014 Jordan Russell <jordan.likes.curry@gmail.com>
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
;; Joystick signals
;;
;;; Code:

(define-module (sly joystick)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (sly event)
  #:use-module (sly signal)
  #:use-module (sly vector)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:re-export ((SDL:joystick-name . joystick-name)
               (SDL:num-joysticks . num-joysticks))
  #:export (enable-joystick
            joystick-num-axes
            joystick-num-buttons
            joystick-axis-hook
            joystick-button-press-hook
            joystick-button-release-hook
            axis-value-raw
            raw-axis-max
            raw-axis-min
            axis-value
            button-down?
            make-directional-signal
            make-directional-signal-raw
            axis-scale))

(define *joysticks* '())

(define (enable-joystick)
  (set! *joysticks*
        (map SDL:joystick-open
             (iota (SDL:num-joysticks)))))

(define (get-joystick idx)
  (list-ref *joysticks* idx))

(define-syntax-rule (js-proc->idx-proc (js-proc . name) doc)
  (define (name idx)
    doc
    (if (> idx (SDL:num-joysticks))
        0
        (js-proc (get-joystick idx)))))

(js-proc->idx-proc (SDL:joystick-num-axes . joystick-num-axes)
                   "Get number of axes of joystick at IDX.")

(js-proc->idx-proc (SDL:joystick-num-buttons . joystick-num-buttons)
                   "Get number of buttons of joystick at IDX.")

(define joystick-axis-hook (make-hook 3))

(register-event-handler
 'joy-axis-motion
 (lambda (e)
   (run-hook joystick-axis-hook
             (SDL:event:jaxis:which e)
             (SDL:event:jaxis:axis e)
             (SDL:event:jaxis:value e))))

(define-record-type <axis-event>
  (make-axis-event which axis value)
  axis-event?
  (which axis-event-joystick)
  (axis axis-event-axis)
  (value axis-event-value))

(define joystick-button-press-hook (make-hook 2))

(register-event-handler
 'joy-button-down
 (lambda (e)
   (run-hook joystick-button-press-hook
             (SDL:event:jbutton:which e)
             (SDL:event:jbutton:button e))))

(define joystick-button-release-hook (make-hook 2))

(register-event-handler
 'joy-button-up
 (lambda (e)
   (run-hook joystick-button-release-hook
             (SDL:event:jbutton:which e)
             (SDL:event:jbutton:button e))))

(define-signal last-axis-event
  (hook->signal joystick-axis-hook 'none
                make-axis-event))

(define raw-axis-min -32768)
(define raw-axis-max 32767)

(define (axis-value-raw idx axis)
  "Create a signal on the axis at AXIS of the joystick at IDX;
joystick axis values are stored in a signed 16 bit integer and so,
values range from [-32768,32767]."
  (signal-map axis-event-value
              (signal-filter
               (lambda (e)
                 (and (axis-event? e)
                      (= (axis-event-joystick e) idx)
                      (= (axis-event-axis e) axis)))
               (make-axis-event idx axis 0)
               last-axis-event)))

(define (make-directional-signal-raw idx x-axis y-axis)
  "Create a signal for a Dpad or Analog stick with X and Y axes;
values range from [-32768,32767]."
  (signal-map vector
              (axis-value-raw idx x-axis)
              (axis-value-raw idx y-axis)))

(define (axis-scale raw-value)
  "Map a RAW-VALUE in [-32768, 32767] to a value in [-1, 1]."
  (define (clamp x)
    (cond ((< (abs x) 1/100) 0)
          ((> x 99/100) 1)
          ((< x -99/100) -1)
          (else x)))
  (clamp (/ raw-value 32768)))

(define (axis-value idx axis)
  "Create a signal for the value of AXIS on joystick IDX;
values are scaled to [-1,1]."
  (signal-map axis-scale (axis-value-raw idx axis)))

(define (make-directional-signal idx x-axis y-axis)
  "Create a signal for a Dpad or Analog stick with X and Y axes;
values are scaled to [-1,1]."
  (signal-map (lambda (v)
                (vector (axis-scale (vx v))
                        (axis-scale (vy v))))
              (make-directional-signal-raw idx x-axis y-axis)))

(define-signal button-last-down
  (hook->signal joystick-button-press-hook 'none
                list))

(define-signal button-last-up
  (hook->signal joystick-button-release-hook 'none
                list))

;; shamelessly copied from keyboard.scm
(define (button-down? idx n)
  "Create a signal for the state of button N on joystick at IDX"
  (define (same-button? l)
    (equal? (list idx n) l))
  (define (button-filter value signal)
    (signal-constant value (signal-filter same-button? #f signal)))
  (signal-merge (button-filter #f button-last-up)
                (button-filter #t button-last-down)))
