(define-library (scheme division)
 (export
  ceiling-quotient
  ceiling-remainder
  ceiling/
  centered-quotient
  centered-remainder
  centered/
  euclidean-quotient
  euclidean-remainder
  euclidean/
  floor-quotient
  floor-remainder
  floor/
  round-quotient
  round-remainder
  round/
  truncate-quotient
  truncate-remainder
  truncate/)
 (import (ds base))
 (begin

  ; private  
  (define (ceiling x)
    (- (floor (- x))))

  (define (ceiling-quotient a b)
    (ceiling (/ a b)))
  
  (define (ceiling-remainder a b)
    (- a (* b (ceiling-quotient a b))))
  
  (define (ceiling/ a b)
    (let ((q (ceiling-quotient a b)))
      (values q (- a (* b q)))))
  
  ; private  
  (define (centered x)
    (floor (+ x 1/2)))
  
  (define (centered-quotient a b)
    (centered (/ a b)))
  
  (define (centered-remainder a b)
    (- a (* b (centered-quotient a b))))

  (define (centered/ a b)
    (let ((q (centered-quotient a b)))
      (values q (- a (* b q)))))
  
  (define (euclidean-quotient a b)
    (* (sign b) (floor (/ a (abs b)))))
  
  (define (euclidean-remainder a b)
    (- a (* b (euclidean-quotient a b))))
  
  (define (euclidean/ a b)
    (let ((q (euclidean-quotient a b)))
      (values q (- a (* b q)))))
  
  (define (floor-quotient a b)
    (floor (/ a b)))
  
  (define (floor-remainder a b)
    (- a (* b (floor-quotient a b))))
  
  (define (floor/ a b)
    (let ((q (floor-quotient a b)))
      (values q (- a (* b q)))))
  
  (define (round-quotient a b)
    (round (/ a b)))
  
  (define (round-remainder a b)
    (- a (* b (round-quotient a b))))
  
  (define (round/ a b)
    (let ((q (round-quotient a b)))
      (values q (- a (* b q)))))

  ; private  
  (define (truncate x)
    (* (sign x) (floor (abs x))))
  
  (define (truncate-quotient a b)
    (truncate (/ a b)))
  
  (define (truncate-remainder a b)
    (- a (* b (truncate-quotient a b))))
  
  (define (truncate/ a b)
    (let ((q (truncate-quotient a b)))
      (values q (- a (* b q)))))

 );begin
);library