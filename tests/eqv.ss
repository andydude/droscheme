(begin
  (assert (eqv? #t #t))
  (assert (eqv? #f #f))
  (assert (not (eqv? #f #t)))
  (assert (not (eqv? #t #f)))
  ; TODO: boolean=?

  (assert (eqv? 'a 'a))
  (assert (not (eqv? 'a 'b)))
  ; TODO: symbol=?

  (assert (eqv? #\a #\a))
  (assert (not (eqv? #\a #\b)))
  ; TODO: char=?

  (assert (eqv? '() '()))
  (assert (not (eqv? '() (cons 1 2))))
  (assert (not (eqv? (cons 1 2) '())))

  (assert (not (eqv? #\c "c")))
  (assert (not (eqv? #\space 32)))

  (assert (let ((x (cons 1 2))) (eqv? x x)))
  (assert (let ((x (vector 1 2))) (eqv? x x)))
  (assert (not (eqv? (cons 1 2) (cons 1 2))))
  (assert (not (eqv? (vector 1 2) (vector 1 2))))
)