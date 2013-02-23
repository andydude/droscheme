; -*- mode: scheme -*-
(define-library (ds any char)
 (begin
  ;; decide on API, no related SRFI

  (define (char->integer ch)
    (call-go int (as ch Char)))

  (define (char? a)
    (:= (_ ok) (as a Char))
    ok)

  (define (char-alphabetic? ch)
    (if1 (not (char? ch))
         (error "char-alphabetic? expected char"))
    (:= cp (as (char->integer ch) int))
    (if1 (<= 65 cp 90) (return #t))
    (if1 (<= 97 cp 122) (return #t))
    #f)

  (define (char-numeric? ch)
    (if1 (not (char? ch))
         (error "char-alphabetic? expected char"))
    (:= cp (as (char->integer ch) int))
    (if1 (<= 48 cp 57) (return #t))
    #f)

  (define (digit-value ch)
    (if1 (not (char-numeric? ch))
         (error "digit-value expected char"))
    (- (char->integer ch) 48))

  (define (integer->char cp)
    (Char (as cp int)))

 );begin
);package
