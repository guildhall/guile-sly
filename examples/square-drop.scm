;;; Square Drop
;;; Copyright (C) 2014 David Thompson <dthompson2@worcester.edu>
;;; Copyright (C) 2014 Jordan Russell <jordan.likes.curry@gmail.com>
;;; Copyright (C) 2015 Amirouche Boubekki <amirouche@hypermove.net>
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
;; Clone of the Magical Drop arcade game
;;
;;; Code:
;;
;; based on 2048 sly code


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
             (sly render model)
             (sly render mesh)
             (sly render shader)                          
             (sly render scene)
             (sly render sprite)
             (sly render texture)
             (sly input keyboard)
             (sly game)
             (sly signal)
             (sly window)
             (sly audio)
             (sly repl))

(use-modules (srfi srfi-1))

;;;
;;; Helpers
;;;

(define (make-tween start end)
  (tween vlerp ease-linear start end 10))


;;;
;;; Constants
;;;

(define square-width 53)

(define background (rgb #xfaf8ef))
(define player-color (rgb #x000000))


;;;
;;; Grid's template(s)
;;;


(define square-index->color
  (list tango-light-butter    ;; 0
        tango-light-orange    
        tango-light-chocolate ;; 3
        tango-light-chameleon
        tango-light-sky-blue  ;; 5
        tango-light-plum
        tango-light-scarlet-red)) ;; 6

(define template-zero
  '((#nil #nil #nil #nil #nil #nil #nil #nil)
    (#nil #nil #nil #nil #nil #nil #nil #nil)
    (#nil #nil #nil #nil #nil #nil #nil #nil)
    (#nil #nil #nil #nil #nil #nil #nil #nil)
    (6 4 6 4 5 5 5 6)
    (0 2 0 4 0 0 1 4)
    (0 2 1 1 5 0 2 4)
    (0 2 0 0 5 1 2 1)
    (1 2 5 0 5 6 6 1)))


;;;
;;; Grid's Square
;;;

(use-modules (srfi srfi-9))

(define-record-type <square>
  (make-square index tween)
  square?
  (index square-index)
  (tween square-tween))


(define null-square (make-square #nil #nil))

(define (pk-grid grid)
  (let loop ((matrix grid)
             (index 0))
    (if (null? matrix)
        grid
        (begin
          (map square-index (car matrix))
          (loop (cdr matrix) (+ 1 index))))))

;; (pk-grid (template->grid template-zero))


(define (square->model square)
  (let ((tween-position (square-tween square))
        (index (square-index square)))
    (if (null? index)
        null-model
        (model-move (tween-position) (model-paint (list-ref square-index->color index) square-sprite)))))

(define (make-square-tween* start end)
  (let* ((absolute-start (v* square-width start))
         (absolute-end (v* square-width end))
         (tween (tween vlerp ease-linear absolute-start absolute-end 15))
         (timer (signal-timer)))
    (lambda ()
      (tween (signal-ref timer)))))


(define (template->grid template)
  (fold (lambda (line j grid)
          (cons (reverse (fold (lambda (index i out)
                                 (let ((start (v* square-width (vector2 j i))))
                                   (if index
                                       (cons (make-square index (make-square-tween* (vector2 i j) (vector2 i j))) out)
                                       (cons null-square out))))
                               '()
                               line
                               (iota (length line))))
                grid))
        '()
        template
        (iota (length template))))

(define (grid-column-first-index grid column)
  "Return the index of the first square of GRID at COLUMN"
  (let loop ((matrix (reverse grid)))
    (if (null? matrix)
        #nil
        (let ((index (square-index (list-ref (car matrix) column))))
          (if (null? index)
              (loop (cdr matrix))
              index)))))


(define (grid-column-count-index grid column index)
  "Return the count of square with equal to INDEX in COLUMN of GRID"
  (let loop ((matrix (reverse grid))
             (count 0))
    (if (null? matrix)
        count
        (let ((square (list-ref (car matrix) column)))
          (if (eq? square null-square)
              (loop (cdr matrix) count)
              (if (eq? (square-index square) index)
                  (loop (cdr matrix) (+ 1 count))
                  count))))))


(define (enumerate-map proc lst)
  (let iter ((lst lst)
             (k 0))
    (if (null? lst)
        '()
        (cons (proc (car lst) k) (iter (cdr lst) (1+ k))))))


;; (pk (enumerate-map (lambda (x i) (cons x i)) (iota 5)))


;; adapted from life.scm
(define (list-substitute list index item)
  "Return a new LIST with element at INDEX replaced with ITEM"
  (append (list-head list index) (cons item (list-tail list (+ 1 index)))))


(define (grid-column-pull grid column index)
  (define pull #t)
  (reverse
   (enumerate-map
    (lambda (squares line)
      (let* ((square (list-ref squares column))
             (target (square-index square)))
        (cond
         ((not pull) squares)
         ((eq? target #nil) squares)
         ((and (eq? target index) (list-substitute squares column null-square)))
         (else (set! pull #f) squares))))
    (reverse grid))))


(define (grid-column-fill grid column index count)
  (define line-count (length grid))
  (define (compute-i line)
    (- line-count line 1))
  
  (enumerate-map
   (lambda (squares line)
     (let* ((square (list-ref squares column))
            (target (square-index square)))
       (cond
        ((and (eq? target #nil) (< 0 count))
         (begin
           (set! count (1- count))
           (list-substitute squares column (make-square index
                                                        (make-square-tween* (vector2 column 0)
                                                                            (vector2 column (compute-i line)))))))
        (else squares))))
   grid))

(define (grid->model grid)
  (list->model (fold (lambda (line out)
                       (fold (lambda (square out)
                               (cons (square->model square) out))
                             out
                             line))
                     '()
                     grid)))

(define (make-grid)
  (template->grid template-zero))

;;;
;;; Position
;;;

(define-record-type <position>
  (make-position index tween timer)
  position?
  (index position-index)
  (tween position-tween)
  (timer position-timer))


(define position-start-vector (vector2 0 10))

(define start-position
  (make-position 0
                 (make-tween position-start-vector position-start-vector)
                 (signal-timer)))


(define position-move-vector (vector2 square-width 0))

(define (position-move position new-index vector)
  (let* ((target (v+ (vector2 0 10) vector))
         (tween (position-tween position))
         (current (tween (signal-ref (position-timer position)))))
    (make-position new-index
                   (make-tween current target)
                   (signal-timer))))

(define (position++ position)
  (let* ((new-index (+ (position-index position) 1))
         (vector-x (v* new-index (vector2 square-width 0))))
    (position-move position new-index vector-x)))

(define (position-- position)
  (let* ((new-index (- (position-index position) 1))
         (vector-x (v* new-index (vector2 square-width 0))))
    (position-move position new-index vector-x)))




;;
;; Player
;;

(define-record-type <player>
  (make-player position square count)
  player?
  (position player-position)
  (square player-square)
  (count player-count))


(define (player-index player)
  (position-index (player-position player)))

(define (new-player)
  (make-player start-position #nil 0))

(define (player-move player direction)
  (match direction
    ('left (if (eq? (player-index player) 0)
               player
               (make-player (position-- (player-position player))
                            (player-square player)
                            (player-count player))))
    ('right (if (eq? (player-index player) 7)
                player
                (make-player (position++ (player-position player))
                             (player-square player)
                             (player-count player))))))

(define (player->model player timer)
  "Create player model based on PLAYER's position"
  (let* ((position (player-position player))
         (tween (position-tween position))
         (timer (position-timer position)))
    (model-move (tween (signal-ref timer)) (model-paint player-color square-sprite))))

;;;
;;; Game
;;;

(define-record-type <game>
  (make-game grid score player)
  game?
  (grid game-grid)
  (score game-score)
  (player game-player))

(define (new-game)
  (make-game (make-grid) 0 (new-player)))


(define (game-pull-column game)
  "Pull the squares if the player has already squares of the same color or no squares
   at all otherwise do nothing"
  (let* ((player (game-player game))
         (position (player-index player))
         (index (grid-column-first-index (game-grid game) position))
         (square (player-square player)))
    (if (or (null? square) (eq? index square))
        (let* ((count (grid-column-count-index (game-grid game) position index))
               (total (+ count (player-count player))))
          (make-game (grid-column-pull (game-grid game) position index)
                     42
                     (make-player (player-position player) index total)))
        ;; return the game unchanged since pull is not possible
        game)))


(define (game-push-column game)
  "Push squares that the player has if any and eliminate squares
   if 3 or more squares of the same color are aligned"
  (let* ((player (game-player game))
         (position (player-index player))
         (square (player-square player))
         (count (player-count player))
         (grid (grid-column-fill (game-grid game) position square count))
         (color (grid-column-first-index grid position)))
    (if (< 0 (player-count player))
        (if (<= 3 (grid-column-count-index grid position color))
            ;; there is more than 3 squares with the same color
            ;; remove them
            ;; FIXME: move this code to next tick, so that the animation
            ;; has time to finish
            (make-game (grid-column-pull grid position color)
                       42
                       (make-player (player-position player) #nil 0))
            (make-game grid 42 (make-player (player-position player) #nil 0)))
        ;; the user has nothing to push
        game)))



;;;
;;; Controls
;;;

;; FIXME: replace with arrow-key signal
(define-signal controls
  (signal-filter
   (lambda (key)
     (any (cut eq? key <>)
          '(left right down up)))
   #f key-last-down))

;;;
;;; state
;;;

(define-signal game-state
  (signal-fold
   (lambda (key previous-state)
     (cond
      ((or (eq? key 'left) (eq? key 'right))
       ;; move player
       (make-game (game-grid previous-state)
                  42
                  (player-move (game-player previous-state) key)))
      ;; pull squares the player has if possible
      ((eq? key 'down) (game-pull-column previous-state))
      ;; push squares the player has if possible
      ((eq? key 'up) (game-push-column previous-state))
      (else previous-state)))
   (new-game)
   controls))

;;;
;;; Rendering
;;;

(open-window)
(enable-fonts)
(enable-audio)


;; (define square-texture (load-texture "tile.png")) ;; 53x53 square (resized sly's 2048 tile.png)
;; (define square-width (texture-width square-texture))
;; (define square-sprite (make-sprite square-texture #:anchor (vector2 0 0)))

(define* (make-square  size)
  (let* ((mesh (build-mesh #(0 3 2 0 2 1)
                           (vector
                            (vector3 0 0 0)
                            (vector3 size 0 0)
                            (vector3 size size 0)
                            (vector3 0 size 0))
                           (let* ((texture null-texture)
                                  (s1 (texture-s1 texture))
                                  (t1 (texture-t1 texture))
                                  (s2 (texture-s2 texture))
                                  (t2 (texture-t2 texture)))
                             (vector (vector2 s1 t1)
                                     (vector2 s2 t2))))))

    (make-model #:mesh mesh #:texture null-texture #:shader (load-default-shader))))
(define square-sprite (make-square 53))

(define window-width 640)
(define window-height 480)

(define-signal game->model
  ;; Generate game's model based on GAME signal
  (signal-map (lambda (game timer)  ;; TBR
                (model-group
                 (player->model (game-player game) timer)
                 (grid->model (game-grid game))))
              game-state (signal-timer)))

(define camera (orthographic-camera window-width
                                    window-height
                                    #:viewport (make-viewport (make-rect 0 0 640 480)
                                                              #:clear-color background)))

(define-signal scene (signal-map (cut make-scene camera <>) game->model))

;;;
;;; Initialization
;;;

(start-sly-repl)

(add-hook! window-close-hook stop-game-loop)

(with-window (make-window #:title "Square Drop (expt alpha 42)")
             (run-game-loop scene))

;;; Local Variables:
;;; compile-command: "../pre-inst-env guile square-drop.scm"
;;; End:
