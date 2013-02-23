; -*- mode: scheme -*-
(define-library (ds seq vector)
 (begin

  ;; vector functions based on SRFI-43

  (define (reverse-list->vector) (void))
  (define (reverse-vector->list) (void))

  (define (vector-append) (void))
  (define (vector-binary-search) (void))
  (define (vector-concatenate) (void))
  (define (vector-copy!) (void))
  (define (vector-copy) (void))
  (define (vector-count) (void))
  (define (vector-empty?) (void))
  (define (vector-fill!) (void))
  (define (vector-fold) (void))
  (define (vector-fold-right) (void))
  (define (vector-for-all) (void)) ; every
  (define (vector-for-any) (void)) ; any
  (define (vector-for-each) (void))
  (define (vector-index) (void))
  (define (vector-index-right) (void))
  (define (vector-length) (void))
  (define (vector-map!) (void))
  (define (vector-map) (void))
  (define (vector-reverse!) (void))
  (define (vector-reverse-copy!) (void))
  (define (vector-reverse-copy) (void))
  (define (vector-skip) (void))
  (define (vector-skip-right) (void))
  (define (vector-swap!) (void))
  (define (vector-unfold) (void))
  (define (vector-unfold-right) (void))

 );begin
);library