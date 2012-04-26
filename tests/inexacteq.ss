(begin
  (assert (eqv? 1.0 1.0))
  (assert (not (eqv? 1.0 1.1)))
  (assert (= 0.0 -0.0))
  (assert (not (equal? +inf.0 -inf.0)))
  (assert (= +inf.0 +inf.0)) ; from R6RS
  (assert (= -inf.0 -inf.0)) ; from R6RS
  (assert (not (= -inf.0 +inf.0))) ; from R6RS
  (assert (< -inf.0 +inf.0))
  (assert (> +inf.0 -inf.0))
  (assert (not (= +nan.0 +nan.0)))
  (assert (not (< +nan.0 +nan.0)))
  (assert (not (> +nan.0 +nan.0)))
  (assert (eqv? +0.0 (/ +inf.0)))
  (assert (eqv? -0.0 (/ -inf.0)))
  (assert (eqv? +inf.0 (/ +0.0)))
  (assert (eqv? -inf.0 (/ -0.0)))
  (assert (eqv? +nan.0 +nan.0))

  ; ieee
  (assert (eqv? +nan.0 (/ 0.0 0.0)))
  (assert (eqv? +nan.0 (+ +nan.0 1.0)))
  (assert (eqv? +nan.0 (- +nan.0 1.0)))
  (assert (eqv? +nan.0 (* +nan.0 1.0)))
  (assert (eqv? +nan.0 (/ +nan.0 1.0)))
  (assert (eqv? +nan.0 (/ +inf.0 +inf.0)))
)
