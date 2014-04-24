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
;; Game data structure.
;;
;;; Code:

(define-module (2d game)
  #:use-module (srfi srfi-9)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (gl)
  #:use-module (2d agenda)
  #:use-module (2d event)
  #:use-module (2d signal)
  #:use-module (2d window)
  #:export (ticks-per-second
            tick-interval
            game-agenda
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
(define game-agenda (make-agenda))
;; This agenda is only ticked when the game loop is in the paused
;; state.  Useful for things like the REPL that should be run even
;; though the game is paused.
(define paused-agenda (make-agenda))

(define (run-game-loop)
  "Start the game loop."
  (parameterize ((game-loop-status 'running)
                 (tick-interval (floor (/ 1000 ticks-per-second))))
    (game-loop (SDL:get-ticks) 0)))

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
  (if (>= lag (tick-interval))
      (begin
        (tick-agenda! game-agenda)
        (update (- lag (tick-interval))))
      lag))

(define (alpha lag)
  (/ lag (tick-interval)))

(define (game-loop previous-time lag)
  "Update game state, and render.  PREVIOUS-TIME is the time in
milliseconds of the last iteration of the game loop."
  (when (game-running?)
    (let* ((current-time (SDL:get-ticks))
           (dt (- current-time previous-time)))
      (process-events)
      (let ((lag (update (+ lag dt))))
        (draw dt (alpha lag))
        (game-loop current-time lag)))))

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
