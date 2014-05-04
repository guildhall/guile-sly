;;; guile-2d
;;; Copyright (C) 2013, 2014 David Thompson <dthompson2@worcester.edu>
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
;; The game loop.
;;
;;; Code:

(define-module (2d game)
  #:use-module (srfi srfi-9)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (gl)
  #:use-module (2d agenda)
  #:use-module (2d event)
  #:use-module (2d math)
  #:use-module (2d signal)
  #:use-module (2d window)
  #:export (tick-interval
            max-ticks-per-frame
            draw-hook
            start-game-loop
            stop-game-loop))

;;;
;;; Game Loop
;;;

;; Update 60 times per second by default.
(define tick-interval (floor (/ 1000 60)))
;; The maximum number of times the game loop will update game state in
;; a single frame.  When this upper bound is reached due to poor
;; performance, the game will start to slow down instead of becoming
;; completely unresponsive and possibly crashing.
(define max-ticks-per-frame 4)
(define draw-hook (make-hook 2))

(define (draw dt alpha)
  "Render a frame."
  (let ((width (signal-ref window-width))
        (height (signal-ref window-height)))
    (gl-viewport 0 0 width height))
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (run-hook draw-hook dt alpha)
  (SDL:gl-swap-buffers))

(define (update lag)
  "Call the update callback. The update callback will be called as
many times as tick-interval can divide LAG. The return value
is the unused accumulator time."
  (define (iter lag ticks)
    (cond ((>= ticks max-ticks-per-frame)
           lag)
          ((>= lag tick-interval)
           (tick-agenda!)
           (iter (- lag tick-interval) (1+ ticks)))
          (else
           lag)))
  (iter lag 0))

(define (alpha lag)
  "Calculate interpolation factor in the range [0, 1] for the
leftover frame time LAG."
  (clamp 0 1 (/ lag tick-interval)))

(define (frame-sleep time)
  "Sleep for the remainder of the frame that started at TIME."
  (let ((t (- (+ time tick-interval)
              (SDL:get-ticks))))
    (usleep (max 0 (* t 1000)))))

(define (game-loop previous-time lag)
  "Update game state, and render.  PREVIOUS-TIME is the time in
milliseconds of the last iteration of the game loop."
  (let* ((current-time (SDL:get-ticks))
         (dt (- current-time previous-time)))
    (process-events)
    (let ((lag (update (+ lag dt))))
      (draw dt (alpha lag))
      (frame-sleep current-time)
      (game-loop current-time lag))))

(define (start-game-loop)
  "Start the game loop."
  (call-with-prompt
   'game-loop-prompt
   (lambda ()
     (game-loop (SDL:get-ticks) 0))
   (lambda (cont callback)
     (when (procedure? callback)
       (callback cont)))))

(define (stop-game-loop)
  "Abort the game loop."
  (abort-to-prompt 'game-loop-prompt #f))
