(define-module (sly mode)
  #:use-module (srfi srfi-9))

(define-record-type <mode>
  (make-mode module vars)
  mode?
  (module mode-module)
  (vars mode-vars))

(define foo-mode
  (make-mode
   (current-module)
   (variables
    ((foo 100)))))

(define-mode foo-mode
  )

(module-define! (current-module) 'foo 10)
foo

(make-variable)
