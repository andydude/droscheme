(use-modules (ice-9 pretty-print))
(load "base-guile.scm")
;(load "ds-sugar.scm")

(define *input-filename*
  (make-parameter "/dev/null"))

(define *input-expr*
  (make-parameter #f))

(define (main-guile)
  (begin
    (pretty-print (read))
    (newline)))

(main-guile)
