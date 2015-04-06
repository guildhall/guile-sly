;;; Mines
;;; Copyright (C) 2014 David Thompson <davet@gnu.org>
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
;; Minesweeper (TM) clone.
;;
;;; Code:

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-26)
             (ice-9 match)
             (sly utils)
             (sly game)
             (sly signal)
             (sly window)
             (sly repl)
             (sly math)
             (sly math rect)
             (sly math transform)
             (sly math vector)
             (sly render camera)
             (sly render color)
             (sly render font)
             (sly render model)
             (sly render sprite)
             (sly input keyboard)
             (sly input mouse))

(set! *random-state* (random-state-from-platform))

;;;
;;; Utils
;;;

(define (list-replace lst k value)
  (append (take lst k) (cons value (drop lst (1+ k)))))

(define (enumerate-map proc lst)
  (define (iter k lst)
    (match lst
      (() '())
      ((x . rest)
       (cons (proc x k) (iter (1+ k) rest)))))

  (iter 0 lst))

(define (compact lst)
  (filter identity lst))

(define nonzero? (negate zero?))

;;;
;;; Model
;;;

(define-record-type <tile>
  (make-tile mine? shown? flag mine-count)
  tile?
  (mine? tile-mine?)
  (shown? tile-shown?)
  (flag tile-flag)
  (mine-count tile-mine-count))

(define (tile-shown-mine? tile)
  (and (tile-shown? tile)
       (tile-mine? tile)))

(define (tile-hidden-mine? tile)
  (and (not (tile-shown? tile))
       (tile-mine? tile)))

(define (tile-shown-not-mine? tile)
  (and (tile-shown? tile)
       (not (tile-mine? tile))))

(define (tile-neighboring-mines? tile)
  (nonzero? (tile-mine-count tile)))

(define tile-show
  (match-lambda
   (($ <tile> mine? _ flag mine-count)
    (make-tile mine? #t flag mine-count))))

(define (tile-mark tile flag)
  (match tile
    (($ <tile> mine? shown? _ mine-count)
     (make-tile mine? shown? flag mine-count))))

(define tile-toggle-flag
  (match-lambda
    (($ <tile> mine? shown? flag mine-count)
     (make-tile mine? shown?
                (match flag
                  ('none 'flag)
                  ('flag 'maybe)
                  ('maybe 'none))
                mine-count))))

(define (set-tile-mine-count tile mine-count)
  (match tile
    (($ <tile> mine? shown? flag _)
     (make-tile mine? shown? flag mine-count))))

(define (tile-flagged? tile)
  (not (eq? (tile-flag tile) 'none)))

(define (tile-flagged-mine? tile)
  (eq? (tile-flag tile) 'flag))

(define (tile-flagged-maybe? tile)
  (eq? (tile-flag tile) 'maybe))

(define (make-board size mine-count)
  (define (random-mine)
    (vector2 (random size) (random size)))

  (let ((mines (let loop ((mines '())
                          (m 0))
                 (if (< m mine-count)
                     (loop (cons (let inner-loop ((mine (random-mine)))
                                   ;; Loop until we don't have a
                                   ;; duplicate mine position.
                                   (if (member mine mines)
                                       (inner-loop (random-mine))
                                       mine))
                                 mines)
                           (1+ m))
                     mines))))

    (define (make-tile* x y)
      (make-tile (list? (member (vector2 x y) mines))
                 #f 'none #f))

    (define (make-row y)
      (list-tabulate size (lambda (x) (make-tile* x y))))

    (list-tabulate size (lambda (y) (make-row y)))))

(define board-ref
  (case-lambda
    ((board x y)
     (list-ref (list-ref board y) x))
    ((board v)
     (list-ref (list-ref board (vy v)) (vx v)))))

(define (board-update board position tile)
  (match position
    (($ <vector2> x y)
     (list-replace board y (list-replace (list-ref board y) x tile)))))

(define (neighbors board pos)
  (let* ((size (length board))
         (area (make-rect 0 0 size size)))
    (chain (list (vector2  1  1)
                 (vector2  1  0)
                 (vector2  1 -1)
                 (vector2  0 -1)
                 (vector2 -1 -1)
                 (vector2 -1  0)
                 (vector2 -1  1)
                 (vector2  0  1))
      (map (cut v+ pos <>))
      (filter (cut rect-contains? area <>)))))

(define (board-reveal board position)
  (let* ((tile (board-ref board position)))
    (cond
     ;; Nothing to do.
     ((or (tile-shown? tile)
          (tile-flagged? tile)
          (board-lose? board))
      board)
     ;; Oops!
     ((tile-mine? tile)
      (board-update board position (tile-show tile)))
     (else
      (let* ((neighbors (neighbors board position))
             ;; Compute bordering mines and reveal tile.
             (mine-count (count (lambda (neighbor)
                                  (tile-mine? (board-ref board neighbor)))
                                neighbors))
             (tile (tile-show (set-tile-mine-count tile mine-count)))
             (board (board-update board position tile)))
        ;; Recursively reveal neighboring tiles if the chosen tile
        ;; does not border a mine.
        (if (zero? mine-count)
            (fold (lambda (pos prev) (board-reveal prev pos))
                  board neighbors)
            board))))))

(define (board-toggle-flag board position)
  (board-update board position
                (tile-toggle-flag (board-ref board position))))

(define (board-win? board)
  (every (lambda (row)
           (every (lambda (tile)
                    (or (tile-shown-not-mine? tile)
                        (tile-hidden-mine? tile)))
                  row))
         board))

(define (board-lose? board)
  (any (cut any tile-shown-mine? <>) board))

;;;
;;; State
;;;

(define resolution (vector2 640 480))
(define tile-size 32)

(define-signal board-size 8)
(define-signal board-area
  (signal-map (lambda (size) (make-rect 0 0 size size)) board-size))

(define-signal center-position
  (signal-map (lambda (board-size)
                (v- (v* 1/2 resolution)
                    (/ (* board-size tile-size) 2)))
              board-size))

(define-signal tile-position
  (signal-map (lambda (p size center)
                (vmap floor (v* (v- p center) (/ 1 tile-size))))
              mouse-position board-size center-position))

(define-signal reveal-clicks
  (chain mouse-last-up
    (signal-filter (cut eq? 'left <>) #f)
    (signal-sample-on tile-position)))

(define-signal flag-clicks
  (chain mouse-last-up
    (signal-filter (cut eq? 'right <>) #f)
    (signal-sample-on tile-position)))

;; User commands.  The first command is the null command to prevent
;; acting upon the initial value of reveal-clicks, which must be
;; ignored.
(define-signal command
  (signal-merge
   (make-signal 'null)
   (signal-map (cut list 'reveal <>) reveal-clicks)
   (signal-map (cut list 'flag <>) flag-clicks)
   (signal-constant 'restart (key-down? 'n))))

(define (make-fresh-board)
  (make-board (signal-ref board-size) 10))

(define (maybe-update-board proc board p)
  (if (rect-contains? (signal-ref board-area) p)
      (proc board p)
      board))

(define-signal board
  (signal-fold (lambda (op board)
                 (match op
                   ('null board)
                   ('restart (make-fresh-board))
                   (('reveal p)
                    (maybe-update-board board-reveal board p))
                   (('flag p)
                    (maybe-update-board board-toggle-flag board p))))
               (make-fresh-board)
               command))

;;;
;;; View
;;;

(open-window)
(enable-fonts)

(define font (load-default-font))

(define sprites
  (map (match-lambda
        ((key . config)
         (cons key (load-sprite
                       (string-append "images/" (assoc-ref config 'name) ".png")
                       #:anchor (assoc-ref config 'anchor)))))
       '((1 .        ((name . "1-mine")
                      (anchor . center)))
         (2 .        ((name . "2-mines")
                      (anchor . center)))
         (3 .        ((name . "3-mines")
                      (anchor . center)))
         (4 .        ((name . "4-mines")
                      (anchor . center)))
         (5 .        ((name . "5-mines")
                      (anchor . center)))
         (6 .        ((name . "6-mines")
                      (anchor . center)))
         (7 .        ((name . "7-mines")
                      (anchor . center)))
         (8 .        ((name . "8-mines")
                      (anchor . center)))
         (mine .     ((name . "mine")
                      (anchor . center)))
         (exploded . ((name . "exploded")
                      (anchor . center)))
         (flag .     ((name . "flag")
                      (anchor . center)))
         (maybe .    ((name . "maybe")
                      (anchor . center)))
         (tile-up .  ((name . "tile-up")
                      (anchor . bottom-left)))
         (tile-down .((name . "tile-down")
                      (anchor . bottom-left))))))

(define (sprite-ref key)
  (assoc-ref sprites key))

(define (tile-base-sprite tile)
  (sprite-ref
   (if (tile-shown? tile)
       'tile-down
       'tile-up)))

(define (tile-overlay-sprite tile)
  (and=> (cond
          ((tile-shown-mine? tile) 'exploded)
          ((tile-flagged-mine? tile) 'flag)
          ((tile-flagged-maybe? tile) 'maybe)
          ((and (tile-shown-not-mine? tile)
                (tile-neighboring-mines? tile))
           (tile-mine-count tile))
          (else #f))
         sprite-ref))

(define draw-tile
  (let ((offset (translate (vector2 (/ tile-size 2) (/ tile-size 2)))))
    (lambda (tile)
      (model-group (tile-base-sprite tile)
                   (let ((overlay (tile-overlay-sprite tile)))
                     (if overlay
                         (model-place offset overlay)
                         null-model))))))

(define-signal board-view
  (signal-map (lambda (board)
                (define (draw-column tile x)
                  (model-move (vector2 (* x tile-size) 0)
                              (draw-tile tile)))

                (define (draw-row row y)
                  (chain (enumerate-map draw-column row)
                    (list->model)
                    (model-move (vector2 0 (* y tile-size)))))

                (list->model (enumerate-map draw-row board)))
              board))

(define-signal status-message
  (signal-map (lambda (board)
                (define (make-message message)
                  (label font message #:anchor 'center))

                (model-move
                 (vector2 (/ (vx resolution) 2) (- (vy resolution) 64))
                 (list->model
                  (cond
                   ((board-lose? board)
                    (list (make-message "GAME OVER - Press N to play again")))
                   ((board-win? board)
                    (list (make-message "YOU WIN! - Press N to play again")))
                   (else '())))))
              board))

(define-signal scene
  (signal-map (lambda (board-view status center-position)
                (model-group status
                             (model-move center-position board-view)))
              board-view status-message center-position))

(define camera
  (orthographic-camera
   (vx resolution) (vy resolution)
   #:viewport (make-viewport (make-rect (vector2 0 0) resolution)
                             #:clear-color tango-dark-plum)))

(define (draw-scene dt alpha)
  (draw-model (signal-ref scene) camera))

;;;
;;; Initialization
;;;

(start-sly-repl)

(add-hook! window-close-hook stop-game-loop)
(add-hook! draw-hook (trampoline draw-scene))

(with-window (make-window #:title "Mines" #:resolution resolution)
  (start-game-loop))

;;; Local Variables:
;;; compile-command: "../../pre-inst-env guile mines.scm"
;;; End:
