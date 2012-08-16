(define-library (ds test)
 (import (scheme base))
 (export outer)
 (begin

  (define (outer name)
    (define (inner name)
      (string-append "Hello " name))
    (write (inner name)))

 );begin
);library