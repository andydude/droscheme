(define-library (ds test)
 (export square)
 (import (only (ds base) *))
 (begin

  (define (square x)
   (* x x))

 );begin
);library