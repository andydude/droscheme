(define-library (ds test)
 (import (scheme base))
 (export greet)
 (begin

  (define (greet name)
    (write (string-append "Hello " name)))

 );begin
);library