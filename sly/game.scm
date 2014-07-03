;;; Sly
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

(define-module (sly game)
  #:use-module (srfi srfi-9)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (gl)
  #:use-module (sly agenda)
  #:use-module (sly event)
  #:use-module (sly math)
  #:use-module (sly signal)
  #:use-module (sly window)
  #:export (draw-hook
            start-game-loop
            stop-game-loop))

;;;
;;; Game Loop
;;;

(define draw-hook (make-hook 2))

(define (interval rate)
  (floor (/ 1000 rate)))

(define* (start-game-loop #:optional #:key
                          (frame-rate 60)
                          (tick-rate 60)
                          (max-ticks-per-frame 4))
  "Start the game loop.  FRAME-RATE specifies the optimal number of
frames to draw per second.  TICK-RATE specifies the optimal game logic
updates per second.  Both FRAME-RATE and TICK-RATE are 60 by default.
MAX-TICKS-PER-FRAME is the maximum number of times the game loop will
update game state in a single frame.  When this upper bound is reached
due to poor performance, the game will start to slow down instead of
becoming completely unresponsive and possibly crashing."
  (let ((tick-interval (interval tick-rate))
        (frame-interval (interval frame-rate)))
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
many times as tick-interval can divide LAG. The return value is the
unused accumulator time."
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
      (let ((t (- (+ time frame-interval)
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

    (call-with-prompt
     'game-loop-prompt
     (lambda ()
       (game-loop (SDL:get-ticks) 0))
     (lambda (cont callback)
       (when (procedure? callback)
         (callback cont))))))

(define (stop-game-loop)
  "Abort the game loop."
  (abort-to-prompt 'game-loop-prompt #f))
