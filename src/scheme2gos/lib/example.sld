(define-library (my example)
 (import (foo fiz))
 (export ex-bar ex-baz)
 (begin
  (define (ex-bar x)
    (foo x))
  (define (ex-baz x y)
    (foo x y))))