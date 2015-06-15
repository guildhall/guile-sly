;;; Sly
;;; Copyright (C) 2015 David Thompson <davet@gnu.org>
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
;; Composite module that exports commonly used public modules.
;;
;;; Code:

(define-module (sly)
  #:use-module (sly window)
  #:use-module (sly render font)
  #:export (sly-init))

(eval-when (eval load compile)
  (begin
    (define %public-modules
      '((sly agenda)
        (sly game)
        (sly signal)
        (sly window)
        (sly repl)
        (sly utils)
        (sly render color)
        (sly render font)
        (sly render sprite)
        (sly render texture)
        (sly render model)
        (sly render camera)
        (sly render scene)
        (sly input keyboard)
        (sly input mouse)
        (sly math)
        (sly math rect)
        (sly math transform)
        (sly math vector)))

    (for-each (let ((i (module-public-interface (current-module))))
                (lambda (m)
                  (module-use! i (resolve-interface m))))
              %public-modules)))

(define* (sly-init #:key (fonts? #t))
  "Initialize Sly's global state, such as the OpenGL context."
  (open-window)
  (when fonts? (enable-fonts)))
