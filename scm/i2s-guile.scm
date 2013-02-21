(use-modules (ice-9 pretty-print))
(load "ds-sugar.scm")

(define (main-guile)
  (let* ((raw (and (< 1 (length (command-line)))
                   (equal? (list-ref (command-line) 1) "-r")))
         (output (sugar-read)))
    (if raw
        (write output)
        (pretty-print output))
    (newline)))

(main-guile)
