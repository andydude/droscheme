(use-modules (ice-9 pretty-print))
(load "base-guile.scm")
(load "ds-common.scm")
(load "ds-function.scm")
(load "ds-library.scm")
(load "scheme2gos-lib.scm")

(define emit-defines emit-defines-export)
(define emit-define-func emit-define-func-compile)
(define emit-import emit-import-compile)
(define emit-package emit-package-compile)

(define (main-guile)
  (let* ((raw (and (< 1 (length (command-line)))
                   (equal? (list-ref (command-line) 1) "-r")))
         (output (compile (read))))
    (if raw
        (write output)
        (pretty-print output))
    (newline)))

(main-guile)
