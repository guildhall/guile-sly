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
;; Game data structure.
;;
;;; Code:

(define-module (2d game)
  #:use-module (srfi srfi-9)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (figl gl)
  #:use-module (2d agenda)
  #:use-module (2d coroutine)
  #:use-module (2d event)
  #:use-module (2d game)
  #:use-module (2d signals)
  #:use-module (2d vector2)
  #:export (ticks-per-second
            tick-interval
            paused-agenda
            draw-hook
            run-game-loop
            quit-game
            pause-game
            resume-game
            game-running?
            game-paused?))

;;;
;;; Game Loop
;;;

;; Possible states are:
;; * stopped
;; * running
;; * paused
(define game-loop-status (make-parameter 'stopped))
(define ticks-per-second 60)
(define tick-interval (make-parameter 0))
(define draw-hook (make-hook 2))
(define accumulator (make-parameter 0))
;; This agenda is only ticked when the game loop is in the paused
;; state.  Useful for things like the REPL that should be run even
;; though the game is paused.
(define paused-agenda (make-agenda))

(define (run-game-loop)
  "Start the game loop."
  (parameterize ((game-loop-status 'running)
                 (tick-interval (floor (/ 1000 ticks-per-second)))
                 (accumulator 0))
    (game-loop (SDL:get-ticks))))

(define (draw dt alpha)
  "Render a frame."
  (set-gl-matrix-mode (matrix-mode modelview))
  (gl-load-identity)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (run-hook draw-hook dt alpha)
  (SDL:gl-swap-buffers))

(define (update)
  "Call the update callback. The update callback will be called as
many times as `tick-interval` can divide ACCUMULATOR. The return value
is the unused accumulator time."
  (while (>= (accumulator) (tick-interval))
      (process-events)
      (tick-agenda! *global-agenda*)
      (accumulator (- (accumulator) (tick-interval)))))

(define (alpha)
  (/ (accumulator) (tick-interval)))

(define (update-and-render dt)
  (update)
  (draw dt (alpha)))

(define (tick dt)
  "Advance the game by one frame."
  (if (game-paused?)
      (begin
        (tick-agenda! paused-agenda)
        (SDL:delay (tick-interval))
        accumulator)
      (catch #t
        (lambda ()
          (update-and-render dt))
        (lambda (key . args)
          (pause-game)
          accumulator)
        (lambda (key . args)
          (display-backtrace (make-stack #t)
                             (current-output-port))))))

(define (game-loop last-time)
  "Update game state, and render. LAST-TIME is the time in
milliseconds of the last iteration of the loop."
  (when (game-running?)
    (let* ((current-time (SDL:get-ticks))
           (dt (- current-time last-time)))
      (accumulator (+ (accumulator) dt))
      (tick dt)
      (game-loop current-time))))

;;;
;;; State management
;;;

(define (game-running?)
  (or (eq? (game-loop-status) 'running)
      (eq? (game-loop-status) 'paused)))

(define (game-paused?)
  (eq? (game-loop-status) 'paused))

(define (pause-game)
  "Pauses the game loop. Useful when developing."
  (game-loop-status 'paused))

(define (resume-game)
  "Resumes the game loop."
  (when (game-paused?)
    (game-loop-status 'running)))

(define (quit-game)
  "Finish the current frame and terminate the game loop."
  (game-loop-status 'stopped))
