;;; Sly
;;; Copyright (C) 2014 David Thompson <davet@gnu.org>
;;;
;;; Sly is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Sly is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Wrapper for functions from the GNU Scientific Library.
;;
;;; Code:

(define-module (sly wrappers gsl)
  #:use-module (system foreign)
  #:export (cblas-row-major
            cblas-no-trans
            cblas-sgemm))

;;;
;;; Constants
;;;

(define cblas-row-major 101)
(define cblas-no-trans 111)

;;;
;;; GSL CBLAS Functions
;;;

(define libgslcblas (dynamic-link "libgslcblas"))

(define-syntax-rule (define-foreign name ret string-name args)
  (define name
    (pointer->procedure ret (dynamic-func string-name libgslcblas) args)))

(define-foreign cblas-sgemm
  void "cblas_sgemm" (list int int int int int int float '*
                           int '* int float '* int))
