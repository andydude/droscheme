(begin
  (define f (open-input-file "test.ss"))
  (assert (equal? (read f) '(define dummy '())))
  (close-input-port f)
)