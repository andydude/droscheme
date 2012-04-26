(begin
  (assert (= 4 (+ 1 (call-with-escape-continuation
                     (lambda (c) (begin (c 3) 2))))))
)