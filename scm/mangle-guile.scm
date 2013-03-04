(load "base-guile.scm")
(load "ds-common.scm")

(define *input-filename*
  (make-parameter "/dev/null"))

(define *input-expr*
  (make-parameter #f))

(define (main-guile)
  (let* ((argv (command-line))
         (str (cadr argv)))
  (begin
    (display (string->mangle str))
    (newline))))

(main-guile)
