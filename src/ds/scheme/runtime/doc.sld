; -*- mode: scheme -*-
(define-library (ds any runtime internal)
 (begin

   (define (empty? a))
   (define (eval exp env))
   (define (eval-literal exp env))
   (define (eval-vector exp env))
   (define (kind-of a))
   (define (kind-string a))
   (define (make-parameter a))
   (define (object-hash a))
   (define (values . r))

 );begin
);library
