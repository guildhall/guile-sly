;;; 2048
;;; Copyright (C) 2014 David Thompson <dthompson2@worcester.edu>
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
;; Clone of the official 2048 game at http://gabrielecirulli.github.io/2048/
;;
;;; Code:

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-11)
             (srfi srfi-26)
             (ice-9 match)
             (sly utils)
             (sly math)
             (sly math rect)
             (sly math transform)
             (sly math tween)
             (sly math vector)
             (sly render camera)
             (sly render color)
             (sly render context)
             (sly render font)
             (sly render group)
             (sly render model)
             (sly render sprite)
             (sly render texture)
             (sly input keyboard)
             (sly game)
             (sly signal)
             (sly window)
             (sly audio)
             (sly repl))

(set! *random-state* (random-state-from-platform))

;;;
;;; Helpers
;;;

(define (enumerate lst)
  (zip (iota (length lst)) lst))

(define (replace-at lst idx item)
  (let-values (((f d)
                (split-at lst idx)))
    (append f (cons item (cdr d)))))

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

(define merge
  (match-lambda
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

(define (board-shift-down board)
  (let-values (((points board)
                (unzip2 (board-shift-left (transpose board)))))
    (zip points (transpose board))))

(define (board-shift-up board)
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

(define (board-insert board)
  (let ((x (random board-size))
        (y (random board-size)))
    (let-values (((f d)
                  (split-at (list-ref board y) x)))
      (if (zero? (car d))
          (replace-at board y
                   (append f (cons (random-tile) (cdr d))))
          (board-insert board)))))

(define (board-find board n)
  (list?
   (any (lambda (row)
          (memq n row))
        board)))

(define (board-win? board)
  (board-find board 2048))

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
  (string-append (getenv "HOME") "/.guile-2048"))

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
          (let ((score (read)))
            (if (number? score) score 0))))
      0))

(define (save-best-score state)
  (with-output-to-file save-file
    (lambda ()
      (write (choose-best-score state)))))

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
(enable-fonts)
(enable-audio)

(define background (rgb #xfaf8ef))

(define tile-texture (load-texture "tile.png"))

(define font (load-default-font 32))

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

(define tile-sprite (make-sprite tile-texture #:anchor (vector2 0 0)))

(define tile-label-cache
  (map (lambda (n)
         (cons n (label font (number->string n) #:anchor 'center)))
       '(2 4 8 16 32 64 128 256 512 1024 2048)))

(define (make-tile x y n)
  (let* ((w (texture-width tile-texture))
         (h (texture-height tile-texture))
         (label (assoc-ref tile-label-cache n))
         (label-color (tile-text-color n))
         (bg-color (tile-bg-color n)))
    (group-move (vector2 (* x w) (* y h))
                (make-group
                 (cons (model-paint bg-color tile-sprite)
                       (if (zero? n)
                           '()
                           (list (group-move (vector2 (/ w 2) (/ h 2))
                                             (group (model-paint label-color
                                                                 label))))))))))

(define window-width 640)
(define window-height 480)
(define board-width
  (* board-size (texture-width tile-texture)))
(define board-height
  (* board-size (texture-height tile-texture)))
(define center-pos
  (vector2 (/ (- window-width board-width) 2) 8))

(define (enumerate-board board)
  (enumerate (map (cut enumerate <>) board)))

(define-signal tiles
  (signal-map (lambda (board)
                (make-group
                 (append-map
                  (match-lambda
                   ((y (row ...))
                    (map (match-lambda
                          ((x n)
                           (make-tile x y n)))
                         row)))
                  (enumerate-board board))))
              board))

(define play-again-font (load-default-font 16))

(define-signal status-message
  (let ((play-again (model-paint black (label play-again-font
                                              "Press N to play again"
                                              #:anchor 'top-center)))
        (game-over (model-paint black (label font "GAME OVER"
                                             #:anchor 'bottom-center)))
        (you-win (model-paint black (label font "YOU WIN!"
                                           #:anchor 'bottom-center))))
    (signal-map
           (lambda (board)
             (let ((message (cond
                             ((board-lose? board) game-over)
                             ((board-win? board) you-win)
                             (else #f))))
               (group-move (vector2 (/ board-width 2)
                                    (/ board-height 2))
                           (make-group (if message
                                           (list message play-again)
                                           '())))))
           board)))

(define instruction-font (load-default-font 16))

(define instruction-text
  "Use the arrow keys to join the numbers and get to the 2048 tile!")

(define-signal instructions
  (group-move (vector2 (/ board-width 2) (- window-height (vy center-pos)))
              (group
               (model-paint text-color-1
                            (label instruction-font instruction-text
                                   #:anchor 'top-center)))))

(define score-header-font (load-default-font 14))
(define score-font (load-default-font 22))

(define (score-label text score x)
  (let* ((duration 15)
         (position-tween (let* ((to (vector2 0 -32))
                                (from (v- to (vector2 0 -8))))
                           (tween vlerp ease-linear from to duration)))
         (color-tween (tween color-lerp ease-linear
                             transparent text-color-1 duration))
         (score (signal-drop-repeats score))
         (header (label score-header-font text #:anchor 'top-center)))
    (signal-map (lambda (score timer)
                  (let ((score (label score-font (number->string score)
                                      #:anchor 'center)))
                    (group-move (vector2 x (- window-height 28))
                                (group
                                 (model-paint text-color-1 header)
                                 (group-move (position-tween timer)
                                             (group
                                              (model-paint (color-tween timer)
                                                           score)))))))
                score
                (signal-drop (lambda (t) (> t duration))
                             0 (signal-since 1 score)))))

(define-signal score
  (score-label "SCORE" (signal-map 2048-score 2048-state) (/ board-width 4)))

(define-signal best-score
  (score-label "BEST" (signal-map 2048-best-score 2048-state)
               (- board-width (/ board-width 4))))

(define-signal 2048-scene
  (signal-map (cut group-move center-pos <>)
              (signal-map group instructions tiles score
                          best-score status-message)))

(define camera
  (orthographic-camera window-width window-height
                       #:viewport (make-viewport (make-rect 0 0 640 480)
                                                 #:clear-color background)))

(define (draw-2048 dt alpha)
  (signal-let ((scene 2048-scene))
    (draw-group scene camera)))

;;;
;;; Initialization
;;;

(start-sly-repl)

(add-hook! window-close-hook stop-game-loop)
(add-hook! draw-hook (trampoline draw-2048))

(with-window (make-window #:title "2048")
  (start-game-loop))

;;; Local Variables:
;;; compile-command: "../../pre-inst-env guile 2048.scm"
;;; End:
