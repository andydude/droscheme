(begin
  (assert (= 4 (+ 1 (call-with-current-continuation
                     (lambda (c) (begin (c 3) 2))))))
)