(define-library (scheme time)
  (export
    current-jiffy
    current-second
    jiffies-per-second)
  (import
   (only
    (builtin)
    current-second))
  (begin
    (define (current-jiffy) (current-second))
    (define (jiffies-per-second) 1)