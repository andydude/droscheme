(begin
  (define x 1)
  (assert (eqv? x 1))
  (set! x 2)
  (assert (eqv? x 2))
)