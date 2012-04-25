(define dummy '())

(define (assert-true name expr)
  (if expr
      (begin
        (display "-- [PASS] ")
        (write name)
        (newline))
      (begin
        (display "-- [FAIL] ")
        (write name)
        (newline))))

(define-syntax assert
  (syntax-rules ()
    ((_ expr)
     (assert-true 'expr expr))))
  