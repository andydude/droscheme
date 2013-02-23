; -*- mode: scheme -*-
(define-library (ds seq runtime)
 (export)
 (begin


;  (define (bytevector-copy bv)
;    (:= rv (make-bytevector (bytevector-length bv)))
;    (bytevector-copy! bv rv)
;    rv)
;
;  (define (bytevector-length bv)
;    (Sint64 (call-go len (as bv Binary))))
;
;  ; nonstandard
;  (define-go (bytevector (args (slice Any)) Any)
;    (return (u8-vector->bytevector (Vector args))))
;
;  (define (bytevector-u8-ref bv k)
;    (as bv Binary (Ref k)))
;
;  (define (bytevector-u8-set! bv k v)
;    (as bv Binary (Set k v)))
;
;  (define (bytevector->u8-list bv)
;    bv)
;
;  (define (bytevector->u8-vector ba)
;    (:= bv (as ba Binary))
;    (:= bl (call-go len bv))
;    (:= rv (as (make-vector (Sint64 bl)) Binary))
;    (for (:= i 0) (< i bl) (++ i)
;         (= ((index rv i)) (index bv i)))
;    rv)
;

  (define (length ls)
    (if1 (not (or (pair? ls) (null? ls)))
         (error "length expected pair or null"))
    (seq-length ls))

  (define (list*->vector z)
    #f)

  (define (list->vector ls)
    (if1 (null? ls)
         (return (Vector #((slice Any)))))
    (if1 (not (pair? ls))
         (error "list->vector expected list"))
	(:= (cur vc) (null) #((slice Any)))
    (for (= cur ls) (pair? cur) (= cur (cdr cur))
         (= vc (call-go append vc (cdr cur))))
    (if1 (not (null? cur))
         (error "list->vector expected null"))
	(Vector vc))

  (define (make-list k)
    (vector->list (make-vector k)))

  (define (make-vector k)
    (:= vc (call-go make (slice Any) (as k int)))
	(Vector vc))

  (define (seq-length sq)
    (as sq Seq (Length)))

  (define (seq-ref sq k #(dv (void)))
    (as sq Seq (RefDefault k dv)))

  (define (seq-set! sq k value)
    (as sq Seq (Set k value)))

  (define (string->list st)
    (vector->list (string->vector st)))

  (define (string->symbol st)
    #(Symbol #:name (as st string)))

  (define (string->vector st)
    (void))

  (define (values . rest)
    gVoid)

  (define (vector) (void))

  (define (vector->list vs)
    (:= (vc ls) (as vs Vector) (null))
    (for (:= i (inline "len(vc) - 1")) (>= i 0) (-- i)
         (= ls (cons (inline "vc[i]") ls)))
    ls)

  (define (vector->list* z)
    #f)

  (define (vector-ref vc k)
    (as vc Vector (Ref k)))

  (define (vector-set! vc k value)
    (if1 (not (vector? vc))
         (error "vector-set! expected vector"))
    (seq-set! vc k value))

  (define (vector? object)
    (:= (_ ok) (as object Vector))
    ok)

  (define (void . rest)
    gVoid)

  (define (void? a)
    (:= (_ ok) (as a Void))
    ok)

 );begin
);library
