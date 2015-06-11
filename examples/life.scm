;;; Life, Sly edition
;;; Copyright (C) 2015 Christopher Allan Webber <cwebber@dustycloud.org>
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

;;; Conway's Game of Life

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 match)
             (ice-9 vlist)
             (sly game)
             (sly window)
             (sly utils)
             (sly signal)
             (sly repl)
             (sly math rect)
             (sly math transform)
             (sly math vector)
             (sly render camera)
             (sly render model)
             (sly render sprite)
             (sly render color)
             (sly render scene)
             (sly input mouse))

;;;
;;; Utils
;;;

(define (vlist-substitute vlist index item)
  "Return a new VLIST with element at INDEX replaced with ITEM"
  (vlist-append
   (vlist-take vlist index)
   (vlist-cons item
               (vlist-drop vlist (+ 1 index)))))

;; Pulled out of mines.scm

(define (enumerate-map proc lst)
  (define (iter k lst)
    (match lst
      (() '())
      ((x . rest)
       (cons (proc x k) (iter (1+ k) rest)))))

  (iter 0 lst))

;;;
;;; Sly stuff starts here
;;;

(load "common.scm")

(define tile-size 32)
(define window-res (vector2 448 480))

(define sprite-cell-alive
  (load-sprite
   "mines/images/tile-down.png"
   #:anchor 'bottom-left))

(define sprite-cell-empty
  (load-sprite
   "mines/images/tile-up.png"
   #:anchor 'bottom-left))

;;;
;;; State
;;;

;; Size of the board
;; @@: Maybe shouldn't be a signal, since
;;     this can't really be dynamically resized at present
(define-signal board-size 14)

(define (make-fresh-board board-size)
  "Make a fresh board (all empty / #f cells)"
  (define (fold-board-size-times thunk)
    (fold
     (lambda (i prev)
       (vlist-cons
        (thunk)
        prev))
     vlist-null
     (iota board-size)))

  (fold-board-size-times
   (lambda ()
     (fold-board-size-times
      (lambda () #f)))))

;; Give a heartbeat indicating it's time to run an evolution on the board
;; (if the simulation is running)
(define-signal time-to-evolve
  (signal-map
   (lambda _
     'evolve)
   (signal-every 20)))

(define (tile-on-board? x y board-size)
  "Is the tile on the board?"
  (define (on-board? pos)
    (if (and (>= pos 0)
             (< pos board-size))
        #t #f))
  (and (on-board? x) (on-board? y)))

(define (tile-at-pos pos board-size tile-size)
  "Find which tile is at the given position"
  (let* ((board-res
          (* board-size tile-size))
         (window-center
          (v* window-res 1/2))
         (first-tile-starts-at
          (v- window-center
              (* board-res 1/2)))
         (pos-relative-to-board
          (v- pos first-tile-starts-at)))
    (cons
     (floor
      (/ (vx pos-relative-to-board)
             tile-size))
     (floor
      (/ (vy pos-relative-to-board)
             tile-size)))))

;; Position of which tile the mouse is currently hovering over
(define-signal mouse-current-tile
  (signal-let ((mouse-position mouse-position)
               (board-size board-size))
    (tile-at-pos mouse-position
                 board-size tile-size)))

;; Mouse left click on a tile
(define-signal toggle-clicks
  (chain mouse-last-up
    (signal-filter (lambda (x) (eq? x 'left)) #f)
    (signal-sample-on mouse-current-tile)))

;; Whether or not the simulation is currently running
(define-signal simulation-running?
  (signal-fold
   (lambda (click currently-running)
     (not currently-running))
   #f
   (signal-filter (lambda (x) (eq? x 'right)) #f
                  mouse-last-up)))

;; Commands that the board should update (tile clicked, new board)
(define-signal board-update
  (signal-merge
   (signal-map (lambda (x)
                 (list 'toggle x))
               toggle-clicks)
   time-to-evolve))

(define (board-cell-ref board board-size row col)
  "Get the value of a cell on a board (#t or #f for aliveness)"
  (cond
   ;; wrap around on rows
   ((< row 0)
    (board-cell-ref board board-size (+ row board-size) col))
   ((>= row board-size)
    (board-cell-ref board board-size (- row board-size) col))

   ;; wrap around on cols
   ((< col 0)
    (board-cell-ref board board-size row (+ col board-size)))
   ((>= col board-size)
    (board-cell-ref board board-size row (- col board-size)))

   (else
    (vlist-ref (vlist-ref board row) col))))

(define (get-neighbors board board-size row col)
  "Get surrounding 8 neighbors of a cell"
  (define (get-cell row col)
    (board-cell-ref board board-size row col))
  (list
   ;; up
   (get-cell (+ row 1) col)
   ;; upper-right
   (get-cell (+ row 1) (+ col 1))
   ;; right
   (get-cell row (+ col 1))
   ;; lower-right
   (get-cell (- row 1) (+ col 1))
   ;; lower
   (get-cell (- row 1) col)
   ;; lower-left
   (get-cell (- row 1) (- col 1))
   ;; left
   (get-cell row (- col 1))
   ;; upper-left
   (get-cell (+ row 1) (- col 1))))

(define (cell-next-val board board-size row col)
  "Get the next value for a cell in an evolution based on its neighbors

If there is no neighbor on an edge, the board wraps around"
  (let* ((neighbors (get-neighbors board board-size
                                   row col))
         (alive-neighbors
          ;; We can use identity, because #t is true!
          (count identity neighbors))
         (currently-alive
          (board-cell-ref board board-size row col)))
    (if currently-alive
        (cond
         ;; Dies by under-population
         ((< alive-neighbors 2) #f)
         ;; healthy population, live
         ((or (= alive-neighbors 2)
              (= alive-neighbors 3))
          #t)
         ;; die from overcrowding
         ((> alive-neighbors 3) #f))
        ;; Since not currently alive,
        ;; we'll spawn if we have three neighbors
        (if (= alive-neighbors 3)
            #t #f))))

(define (evolve-board current-board board-size)
  "Return new evolved board based on CURRENT-BOARD for BOARD-SIZE"
  ;; loop on rows
  (list->vlist
   (map
    ;; loop on cols
    (lambda (row)
      (list->vlist
       (map
        (lambda (col)
          (cell-next-val current-board board-size row col))
        (iota board-size))))
    (iota board-size))))

(define (update-board-by-tile-toggle current-board row col)
  "Update CURRENT-BOARD by returning new board with ROW COL tile toggled"
  (let* ((current-row
          (vlist-ref current-board row))
         (current-status
          (vlist-ref current-row col)))
    (vlist-substitute
     current-board row
     (vlist-substitute
      current-row col
      (not current-status)))))

;; The actual game board structure
(define-signal board
  (signal-fold
   (lambda (update board-size running? current-board)
     (match update
       ('evolve
        (if running?
            (evolve-board current-board board-size)
            current-board))
       (('toggle (col . row))
        (if (tile-on-board? col row board-size)
            (update-board-by-tile-toggle
             current-board row col)
            current-board))
       (anything-else
        (format #t "Unhandled: ~s\n" anything-else)
        current-board)))
   (make-fresh-board (signal-ref board-size))
   board-update board-size simulation-running?))

;; Determine a tile's position
(define (tile-pos row col board-size tile-size)
  (v-
   (v+
    (vector2 (* col tile-size)
             (* row tile-size))
    (v* window-res 1/2))
   (vector2
    (/ (* board-size tile-size) 2)
    (/ (* board-size tile-size) 2))))

;; Model of the tile grid
(define-signal tiles-view
  (signal-let ((board board)
               (board-size board-size))
    (list->model
     (enumerate-map
      (lambda (row row-count)
        (list->model
         (enumerate-map
          (lambda (tile-alive col-count)
            (model-move (tile-pos row-count col-count
                                  board-size tile-size)
                        (if tile-alive
                            sprite-cell-alive
                            sprite-cell-empty)))
          (vlist->list row))))
      ;; FIXME:
      ;; This slows things down more than it should have to
      ;; we should map natively on the vlist
      (vlist->list board)))))

(define-signal camera
  (signal-let ((running? simulation-running?))
    (orthographic-camera
     (vx window-res) (vy window-res)
     #:viewport (make-viewport (make-rect (vector2 0 0) window-res)
                               #:clear-color (if running?
                                                 tango-dark-chameleon
                                                 tango-dark-scarlet-red)))))

(define-signal scene
  (signal-let ((model  tiles-view)
               (camera camera))
    (make-scene camera model)))

;;;
;;; Initialization
;;;

(add-hook! window-close-hook stop-game-loop)

(with-window (make-window #:title "Life (right click to start/stop)"
                          #:resolution window-res)
  (run-game-loop scene))
