                                        ; primitives.scm

(define-module (sly primitives)
  #:use-module (srfi srfi-1)
  #:use-module (gl)
  #:use-module (gl contrib packed-struct)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (srfi srfi-9)
  #:use-module (sly color)
  #:use-module (sly vector)
  #:export (make-primitive
            primitive?
            primitive-vectors
            primitive-mode
            make-rectangle
            make-triangle
            make-line
            make-line-strip
            draw-primitive
            draw-outline
            draw-points))

;;;
;;; Vertex arrays for primitives.
;;;

(define-packed-struct primitive-vertex
  (x float)
  (y float))

(define (make-vertex-array vertices)
  "Create a vertex-array usable by (gl-draw-arrays) from `vertices`."
  (define (for-each-counter f l)
    (fold (lambda (elem counter)
            (f elem counter)
            (+ counter 1))
          0
          l))
  (define (pack-array array offset)
    (for-each-counter (lambda (vertex offset)
                        (pack array offset primitive-vertex
                              (vx vertex)
                              (vy vertex)))
                      vertices))
  (let ((vertex-list (make-packed-array primitive-vertex
                                        (length vertices))))
    (pack-array vertex-list 0)
    vertex-list))

(define (draw-vertices vertex-array count mode)
  "Draw `count` vertices from `vertex-array` using the mode `mode`.
`vertex-array` should be an array of packed `primitive-vertex` structs."
  (let ((pointer-type (tex-coord-pointer-type float)))
    (gl-enable-client-state (enable-cap vertex-array))
    (set-gl-vertex-array pointer-type
                         vertex-array
                         2
                         #:stride (packed-struct-size primitive-vertex)
                         #:offset (packed-struct-offset primitive-vertex x))
    (gl-draw-arrays mode 0 count)
    (gl-disable-client-state (enable-cap vertex-array))))

;;;
;;; Primitives
;;;

;; Object for drawing lines, polygons, points, etc.
(define-record-type <primitive>
  (%make-primitive color vectors vertex-array mode count)
  primitive?
  (vectors primitive-vectors)
  (vertex-array primitive-vertex-array)
  (mode primitive-mode)
  (count primitive-vector-count))

(define* (draw-primitive primitive #:optional mode)
  "Draw the primitive `primitive` using the optional mode `mode`."
  (let ((mode (if mode mode (primitive-mode primitive)))
        (vertex-array (primitive-vertex-array primitive)))
    (use-color (primitive-color primitive))
    (draw-vertices vertex-array
                   (primitive-vector-count primitive)
                   mode)))

(define (draw-outline primitive)
  "Draw the vertices of `primitive` as a line-loop,
which effectively draws the outline of a shape."
  (draw-primitive primitive
                  (begin-mode line-loop)))

(define (draw-points primitive)
  "Draw the vertices of `primitive` as points."
  (draw-primitive primitive
                  (begin-mode points)))

(define (make-primitive color vectors mode)
  "Create a new primitive object. `vectors` should be a list
of vectors that will be drawn when the object is drawn, and
`mode` should be a value from (begin-mode) (quads, triangles,
etc.)."
  (%make-primitive color vectors (make-vertex-array vectors)
                   mode (length vectors)))

(define (make-polygon color vectors)
  "Take a list of vectors and return a primitive object."
  (make-primitive color vectors (begin-mode polygon)))

(define (make-rectangle color v1 v2 v3 v4)
  (make-primitive color (list v1 v2 v3 v4) (begin-mode quads)))

(define (make-triangle color v1 v2 v3)
  (make-primitive color (list v1 v2 v3) (begin-mode triangles)))

(define (make-line-strip color vectors)
  (if (< (length vectors) 2)
      (error "Not enough vectors given to draw a line!")
      (make-primitive color vectors (begin-mode line-strip))))

(define (make-line color v1 v2)
  (make-line-strip color (list v1 v2)))
