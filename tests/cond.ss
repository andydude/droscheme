(begin
  (assert (cond (#t #t)))
  (assert (not (cond (#t #f))))
  (assert (= (cond (#f 3) (#t 4)) 4))
)