;(load "templates.scm")
;(define emit emit-expr)
(load "base-guile.scm")
(load "common.scm")
(define emit-defines emit-defines-export)
(define emit-define-func emit-define-func-compile)
(define emit-import emit-import-compile)
(define emit-package emit-package-compile)

(define (main-guile)
  (begin
    (*input-filename* (list-ref (command-line) 1))
    ;(*package-name* (basename (*input-filename*)))
    ;(*package-path* (string-replace (*package-name*) "_" "/"))
    (display (emit (call-with-input-file (*input-filename*) read)))
    (newline)))

(main-guile)
