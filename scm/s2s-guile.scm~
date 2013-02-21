(load "base-guile.scm")
(load "gos2go-lib.scm")

(define *input-filename*
  (make-parameter "/dev/null"))

(define *input-expr*
  (make-parameter #f))

(define (main-guile)
  (begin
    ;(*input-filename* (list-ref (command-line) 1))
    ;(*input-expr* (call-with-input-file (*input-filename*) read))
    (display (emit-expr (read)))
    (newline)))

(main-guile)
