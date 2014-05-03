#! /usr/bin/env guile
!#

;;; guile-2048
;;; Copyright (C) 2014 David Thompson <dthompson2@worcester.edu>
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
;; Clone of the official 2048 game at http://gabrielecirulli.github.io/2048/
;;
;;; Code:

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-11)
             (srfi srfi-26)
             (srfi srfi-42)
             (ice-9 match)
             (ice-9 rdelim)
             (gl)
             (2d audio)
             (2d color)
             (2d font)
             (2d game)
             (2d keyboard)
             (2d rect)
             (2d signal)
             (2d sprite)
             (2d texture)
             (2d vector2)
             (2d window)
             (2d repl))
;;;
;;; Helpers
;;;

(define* (flat-map proc lst . rest)
  (concatenate (apply map proc lst rest)))

(define (enumerate lst)
  (zip (iota (length lst)) lst))

;;;
;;; Game Board
;;;

(define board-size 4)

(define (double x)
  (* x 2))

(define (strip-zeros lst)
  (delete 0 lst))

(define (pad-zeros lst size)
  (append lst (make-list (max (- size (length lst)) 0) 0)))

(define (merge lst)
  (match lst
    ((x x . rest)
     (cons (double x) (merge rest)))
    ((x . rest)
     (cons x (merge rest)))
    (_ '())))

(define (points a b)
  (define (iter a b p)
    (cond ((or (null? a)
               (null? b))
           p)
          ((= (car a) (car b))
           (iter (cdr a) (cdr b) p))
          (else
           (iter (cddr a) (cdr b) (+ p (car b))))))
  (iter a b 0))

;; Merge list and accumulate points.
(define (merge-row row)
  (let* ((stripped (strip-zeros row))
         (merged (merge stripped)))
    (list (points stripped merged)
          (pad-zeros merged board-size))))

(define (transpose board)
  (if (null? (car board))
      '()
      (cons (map car board)
            (transpose (map cdr board)))))

(define (board-shift-left board)
  (map merge-row board))

(define (board-shift-right board)
  (map (lambda (row)
         (let ((merged (merge-row (reverse row))))
           (list (first merged) (reverse (second merged)))))
       board))

(define (board-shift-up board)
  (let-values (((points board)
                (unzip2 (board-shift-left (transpose board)))))
    (zip points (transpose board))))

(define (board-shift-down board)
  (let-values (((points board)
                (unzip2 (board-shift-right (transpose board)))))
    (zip points (transpose board))))

(define (board-shift board direction)
  (match direction
    ('up
     (board-shift-up board))
    ('down
     (board-shift-down board))
    ('left
     (board-shift-left board))
    ('right
     (board-shift-right board))
    (_ board)))

(define (board-shift-and-accum-points board direction)
  (let-values (((points board)
                (unzip2 (board-shift board direction))))
    (values (reduce + 0 points)
            board)))

(define (random-tile)
  (list-ref '(2 4) (random 2)))

;; So gross.
(define (board-insert board)
  (let ((x (random board-size))
        (y (random board-size)))
    (if (zero? (list-ref (list-ref board y) x))
        (append (take board y)
                (let ((rows (drop board y)))
                  (cons (let ((cells (car rows)))
                          (append (take cells x)
                                  (let ((rest (drop cells x)))
                                    (cons (random-tile)
                                          (cdr rest)))))
                        (cdr rows))))
        (board-insert board))))

(define (board-find board n)
  (list?
   (any (lambda (row)
          (memq n row))
        board)))

(define (board-win? board)
  (board-find (make-board) 2048))

(define (board-lose? board)
  (define (full? row)
    (define (iter row prev)
      (cond ((null? row)
             #t)
            ((or (zero? (car row))
                 (= (car row) prev))
             #f)
            (else
             (iter (cdr row) (car row)))))
    (iter row 0))
  (and (every full? board)
       (every full? (transpose board))))

(define null-board
  '((0 0 0 0)
    (0 0 0 0)
    (0 0 0 0)
    (0 0 0 0)))

(define (make-board)
  (board-insert (board-insert null-board)))

;;;
;;; Game State
;;;

(define save-file
  (string-join (list (getenv "HOME") ".guile-2048")
               file-name-separator-string))

(define-record-type <2048>
  (make-2048 board score best-score)
  2048?
  (board 2048-board)
  (score 2048-score)
  (best-score 2048-best-score))

(define* (new-game #:optional (previous #f))
  (let ((best-score (if previous
                        (choose-best-score previous)
                        (load-best-score))))
    (make-2048 (make-board) 0 best-score)))

(define (load-best-score)
  (if (file-exists? save-file)
      (with-input-from-file save-file
        (lambda ()
          (let ((score (string->number (read-string))))
            (if (number? score) score 0))))
      0))

(define (save-best-score state)
  (with-output-to-file save-file
    (lambda ()
      (format #t "~d" (choose-best-score state)))))

(define (choose-best-score state)
  (max (2048-score state) (2048-best-score state)))

(define-signal controls
  (signal-filter
   (lambda (key)
     (any (cut eq? key <>)
          '(up down left right n)))
   #f key-last-down))

(define-signal 2048-state
  (signal-fold
   (lambda (key prev)
     (if (eq? key 'n)
         (new-game prev)
         (let-values (((points new-board)
                       (board-shift-and-accum-points (2048-board prev) key)))
           (let ((score (+ (2048-score prev) points)))
             ;; Only insert a new tile if there's room and the board
             ;; was actually shifted.
             (if (and (not (equal? (2048-board prev) new-board))
                      (board-find new-board 0))
                 (make-2048 (board-insert new-board) score
                            (2048-best-score prev))
                 (make-2048 new-board score (2048-best-score prev)))))))
   (new-game)
   controls))

;; For convenience
(define-signal board
  (signal-map 2048-board 2048-state))

(define-signal score-saver
  (signal-tap (lambda (state)
                (when (board-lose? (2048-board state))
                  (save-best-score state)))
              2048-state))

;;;
;;; Rendering
;;;

(open-window)
(enable-sprites)
(enable-fonts)
(enable-audio)

(define background (rgb #xfaf8ef))

(set-gl-clear-color (color-r background)
                    (color-g background)
                    (color-b background)
                    (color-a background))

(define tile-texture (load-texture "tile.png"))

(define font (load-default-font 32))

(define-record-type <tile>
  (%make-tile background label)
  tile?
  (background tile-background)
  (label tile-label))

(define text-color-1 (rgb #x776e65))
(define text-color-2 (rgb #xf9f6f2))

(define tile-properties
  `((0    . ((bg-color . ,(rgba #xeee4daaa))
             (text-color . ,text-color-1)))
    (2    . ((bg-color . ,(rgb #xeee4da))
             (text-color . ,text-color-1)))
    (4    . ((bg-color . ,(rgb #xede0c8))
             (text-color . ,text-color-1)))
    (8    . ((bg-color . ,(rgb #xf2b179))
             (text-color . ,text-color-2)))
    (16   . ((bg-color . ,(rgb #xf59563))
             (text-color . ,text-color-2)))
    (32   . ((bg-color . ,(rgb #xf67c5f))
             (text-color . ,text-color-2)))
    (64   . ((bg-color . ,(rgb #xf65e3b))
             (text-color . ,text-color-2)))
    (128  . ((bg-color . ,(rgb #xedcf72))
             (text-color . ,text-color-2)))
    (256  . ((bg-color . ,(rgb #xedcc61))
             (text-color . ,text-color-2)))
    (512  . ((bg-color . ,(rgb #xedc850))
             (text-color . ,text-color-2)))
    (1024 . ((bg-color . ,(rgb #xedc53f))
             (text-color . ,text-color-2)))
    (2048 . ((bg-color . ,(rgb #xedc22e))
             (text-color . ,text-color-2)))))

(define (tile-bg-color n)
  (assoc-ref (assoc-ref tile-properties n) 'bg-color))

(define (tile-text-color n)
  (assoc-ref (assoc-ref tile-properties n) 'text-color))

(define (make-tile x y n)
  (let* ((w (texture-width tile-texture))
         (h (texture-height tile-texture))
         (background
          (make-sprite tile-texture
                       #:position (center
                                   (vector2
                                    (* x w)
                                    (* y h)))
                       #:color (tile-bg-color n)
                       #:anchor null-vector2))
         (label
          (make-label font
                      (if (zero? n) " " (number->string n))
                      (center
                       (vector2 (+ (* x w)
                                   (/ w 2))
                                (+ (* y h)
                                   (/ h 2))))
                      #:color (tile-text-color n)
                      #:anchor 'center)))
    (%make-tile background label)))

(define (draw-tile tile)
  (draw-sprite (tile-background tile))
  (draw-label (tile-label tile)))

(define window-width 640)
(define window-height 480)
(define board-width
  (* board-size (texture-width tile-texture)))
(define board-height
  (* board-size (texture-height tile-texture)))
(define center-pos
  (vector2 (/ (- window-width board-width) 2)
           (- window-height board-height 8)))

(define (center v)
  (v+ v center-pos))

(define (enumerate-board board)
  (enumerate (map (cut enumerate <>) board)))

;; Transform board into a list of tile objects.
(define-signal tiles
  (signal-map
   (lambda (board)
     (flat-map
      (lambda (row)
        (let ((y (first row))
              (row (second row)))
          (map (lambda (cell)
                 (let ((x (first cell))
                       (n (second cell)))
                   (make-tile x y n)))
               row)))
      (enumerate-board board)))
   board))

(define-signal status
  (signal-map
   (lambda (board)
     (let ((message (cond ((board-lose? board) "GAME OVER")
                          ((board-win? board) "YOU WIN!")
                          (else ""))))
       (make-label font message
                   (center
                    (vector2 (/ board-width 2)
                             (/ board-height 2)))
                   #:color black
                   #:anchor 'bottom-center)))
   board))

(define play-again-font (load-default-font 16))

(define-signal play-again-message
  (signal-map
   (lambda (board)
     (make-label play-again-font
                 (if (or (board-lose? board)
                         (board-win? board))
                     "Press N to play again"
                     "")
                 (center
                  (vector2 (/ board-width 2)
                           (/ board-height 2)))
                 #:color black
                 #:anchor 'top-center))
   board))

(define instruction-font (load-default-font 16))

(define instructions
  (make-label instruction-font
              "Use the arrow keys to join the numbers and get to the 2048 tile!"
              (vector2 (/ window-width 2) 0)
              #:color text-color-1
              #:anchor 'top-center))

(define score-header-font (load-default-font 14))
(define score-font (load-default-font 22))

(define score-header
  (make-label score-header-font
              "SCORE"
              (vector2 (+ (vx center-pos) (/ board-width 4)) 24)
              #:color text-color-1
              #:anchor 'top-center))

(define-signal score
  (signal-map
   (lambda (state)
     (make-label score-font
                 (format #f "~d" (2048-score state))
                 (vector2 (vx (label-position score-header))
                          (+ (vy (label-position score-header)) 32))
                 #:color text-color-1
                 #:anchor 'center))
   2048-state))

(define best-score-header
  (make-label score-header-font
              "BEST"
              (vector2 (+ (vx center-pos) (- board-width (/ board-width 4))) 24)
              #:color text-color-1
              #:anchor 'top-center))

(define-signal best-score
  (signal-map
   (lambda (state)
     (make-label score-font
                 (format #f "~d" (2048-best-score state))
                 (vector2 (vx (label-position best-score-header))
                          (+ (vy (label-position best-score-header)) 32))
                 #:color text-color-1
                 #:anchor 'center))
   2048-state))

(define (render)
  (for-each draw-tile (signal-ref tiles))
  (draw-label instructions)
  (draw-label score-header)
  (draw-label best-score-header)
  (draw-label (signal-ref score))
  (draw-label (signal-ref best-score))
  (draw-label (signal-ref status))
  (draw-label (signal-ref play-again-message)))

;;;
;;; Initialization
;;;

(start-2d-repl)

(add-hook! window-close-hook stop-game-loop)
(add-hook! draw-hook (lambda (dt alpha) (render)))

(with-window (make-window #:title "2048")
  (start-game-loop))