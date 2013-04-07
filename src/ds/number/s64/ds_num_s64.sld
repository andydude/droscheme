(define-library
 (ds num s64)
 (import)
 (export
  *
  */carry
  +
  +/carry
  -
  -/carry
  <
  <=
  =
  >
  >=
  abs
  arithmetic-shift-left
  arithmetic-shift-right
  bit-and
  bit-count
  bit-eqv
  bit-field
  bit-if
  bit-implies
  bit-nif
  bit-nimplies
  bit-not
  bit-or
  bit-set?
  bit-xor
  complex?
  copy-bit
  copy-bit-field
  euc%
  euc/
  even?
  exact?
  first-bit-set
  greatest
  inexact?
  integer?
  least
  length
  logical-shift-left
  logical-shift-right
  max
  min
  negative?
  number?
  odd?
  positive?
  rational?
  real?
  reverse-bit-field
  rna%
  rna/
  rnn%
  rnn/
  rnp%
  rnp/
  rnz%
  rnz/
  rotate-bit-field
  rta%
  rta/
  rte%
  rte/
  rtn%
  rtn/
  rtp%
  rtp/
  rtz%
  rtz/
  shift
  shift-left
  shift-right
  width
  zero?)
 (begin

  (define (+ . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:return 0))
     ((1) (go:return (go:as (go:index rest 0) go:int64)))
     ((2) (go:return (go:+ (go:as (go:index rest 0) go:int64)
			   (go:as (go:index rest 1) go:int64)))))
    ;; iterate
    (go::= rv (go:as (go:index rest 0) go:int64))
    (go:for (go::= i 1) (go:< i (go:len rest)) (go:++ i)
	    (go::= it (go:as (go:index rest i) go:int64))
	    (go:+= rv it))
    rv)

  (define (- . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:return (go:- (go:as (go:index rest 0) go:int64))))
     ((2) (go:return (go:- (go:as (go:index rest 0) go:int64)
			   (go:as (go:index rest 1) go:int64)))))
    ;; recur
    (- (go:index rest 0) (go:apply + (go:index rest 1 #f))))

  (define (* . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:return 1))
     ((1) (go:return (go:index rest 0)))
     ((2) (go:return (go:* (go:as (go:index rest 0) go:int64)
			   (go:as (go:index rest 1) go:int64)))))
    ;; iterate
    (go::= rv (go:as (go:index rest 0) go:int64))
    (go:for (go::= i 1) (go:< i (go:len rest)) (go:++ i)
	    (go::= it (go:as (go:index rest i) go:int64))
	    (go:*= rv it))
    rv)

;(fx+/carry fx1 fx2 fx3)‌‌
;Returns the two fixnum results of the following computation:
;
;(let* ((s (+ fx1 fx2 fx3))
;       (s0 (mod0 s (expt 2 (fixnum-width))))
;       (s1 (div0 s (expt 2 (fixnum-width)))))
;  (values s0 s1))
  (define (+/carry a b c)
    #f)


;(fx-/carry fx1 fx2 fx3)‌‌
;Returns the two fixnum results of the following computation:
;
;(let* ((d (- fx1 fx2 fx3))
;       (d0 (mod0 d (expt 2 (fixnum-width))))
;       (d1 (div0 d (expt 2 (fixnum-width)))))
;  (values d0 d1))
  (define (-/carry a b c)
    #f)


;(fx*/carry fx1 fx2 fx3)‌‌
;Returns the two fixnum results of the following computation:
;
;(let* ((s (+ (* fx1 fx2) fx3))
;       (s0 (mod0 s (expt 2 (fixnum-width))))
;       (s1 (div0 s (expt 2 (fixnum-width)))))
;  (values s0 s1))
  (define (*/carry a b c)
    #f)

  (define (= . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:== (go:as (go:index rest 0) go:int64)
			    (go:as (go:index rest 1) go:int64)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
	    (go:unless (go:== (go:as (go:index rest i) go:int64)
			      (go:as (go:index rest (go:+ i 1)) go:int64))
		       (go:return #f)))
    #t)

  (define (< . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:< (go:as (go:index rest 0) go:int64)
			   (go:as (go:index rest 1) go:int64)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
	    (go:unless (go:< (go:as (go:index rest i) go:int64)
			     (go:as (go:index rest (go:+ i 1)) go:int64))
		       (go:return #f)))
    #t)

  (define (<= . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:<= (go:as (go:index rest 0) go:int64)
			    (go:as (go:index rest 1) go:int64)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
	    (go:unless (go:<= (go:as (go:index rest i) go:int64)
			      (go:as (go:index rest (go:+ i 1)) go:int64))
		       (go:return #f)))
    #t)

  (define (> . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:> (go:as (go:index rest 0) go:int64)
			   (go:as (go:index rest 1) go:int64)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
	    (go:unless (go:> (go:as (go:index rest i) go:int64)
			     (go:as (go:index rest (go:+ i 1)) go:int64))
		       (go:return #f)))
    #t)

  (define (>= . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:>= (go:as (go:index rest 0) go:int64)
			    (go:as (go:index rest 1) go:int64)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
	    (go::= a (go:as (go:index rest i) go:int64))
	    (go::= b (go:as (go:index rest (go:+ i 1)) go:int64))
	    (go:unless (go:>= a b) (go:return #f)))
    #t)

  (define (abs a)
    (go:when (go:< (go:as a go:int64) 0)
	     (go:return (- a)))
    a)

  (define (bit-and . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:return (go:int64 -1)))
     ((1) (go:return (go:as (go:index rest 0) go:int64))))
    ;; iterate
    (go::= rv (go:as (go:index rest 0) go:int64))
    (go:for (go::= i 1) (go:< i (go:len rest)) (go:++ i)
	    (go::= it (go:as (go:index rest i) go:int64))
	    (go:bitwise-and= rv it))
    rv)

  (define (bit-count)
    #f)


  (define (bit-eqv . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:return (go:int64 -1)))
     ((1) (go:return (go:as (go:index rest 0) go:int64))))
    ;; iterate
    (go::= rv (go:as (go:index rest 0) go:int64))
    (go:for (go::= i 1) (go:< i (go:len rest)) (go:++ i)
	    (go::= it (go:as (go:index rest i) go:int64))
	    (go:= rv (go:bitwise-not (go:bitwise-xor rv it))))
    rv)

  (define (bit-field)
    #f)

  ;(define (bit-nimplies . rest)
  ;  ;; fast
  ;  (go:case! (go:len rest)
  ;   ((0) (go:return (go:int64 -1)))
  ;   ((1) (go:return (go:as (go:index rest 0) go:int64))))
  ;  ;; iterate
  ;  (go::= rv (go:as (go:index rest 0) go:int64))
  ;  (go:for (go::= i 1) (go:< i (go:len rest)) (go:++ i)
  ;      (go::= it (go:as (go:index rest i) go:int64))
  ;      (go:bitwise-but= rv it))
  ;  rv)

  (define (bit-if a b)
    (bit-not (bit-nimplies b a)))

  (define (bit-implies a b)
    (bit-not (bit-nimplies a b)))

  (define (bit-nif a b)
    (bit-nimplies b a))

  (define (bit-nimplies a b)
    (go:bitwise-but (go:as a go:int64)
                    (go:as b go:int64)))

  (define (bit-not a)
    (go:bitwise-not (go:as a go:int64)))

  (define (bit-or . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:return (go:int64 -1)))
     ((1) (go:return (go:as (go:index rest 0) go:int64))))
    ;; iterate
    (go::= rv (go:as (go:index rest 0) go:int64))
    (go:for (go::= i 1) (go:< i (go:len rest)) (go:++ i)
	    (go::= it (go:as (go:index rest i) go:int64))
	    (go:bitwise-or= rv it))
    rv)

  (define (bit-set?)
    #f)

  (define (bit-xor . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:return (go:int64 -1)))
     ((1) (go:return (go:as (go:index rest 0) go:int64))))
    ;; iterate
    (go::= rv (go:as (go:index rest 0) go:int64))
    (go:for (go::= i 1) (go:< i (go:len rest)) (go:++ i)
	    (go::= it (go:as (go:index rest i) go:int64))
	    (go:bitwise-xor= rv it))
    rv)

  (define (complex? a)
    #t)

  (define (copy-bit)
    #f)

  (define (copy-bit-field)
    #f)

  ;; primitives for r7rs:euclidean/, r6rs:div-and-mod
  (define (euc/ a b)
    (rtz/ (- a (euc% a b)) b))

  (define (euc% a b)
    (go:cond!
     ((go:as (negative? (rtz% a b)) go:bool)
      (go:return (+ (rtz% a b) (abs b)))))
    (rtz% a b))

  (define (even? a)
    (go:== (go:% (go:as a go:int64) 2) 0))

  (define (exact? a)
    #t)

  (define (first-bit-set)
    #f)

  (define (greatest)
    (go:int64 9223372036854775807))
;    (go:int64 #x7fffffffffffffff))

  (define (inexact? a)
    #f)

  (define (integer? a)
    #t)

  (define (least)
    (go:int64 -9223372036854775808))
;    (go:int64 -#x8000000000000000))

  ;; TODO
  (define (length a)
    (if (negative? a)
	(length (- a))
	#t))

  (define (max . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:return (go:as (go:index rest 0) go:int64))))
    ;; iterate
    (go::= rv (go:as (go:index rest 0) go:int64))
    (go:for (go::= i 1) (go:< i (go:len rest)) (go:++ i)
	    (go::= it (go:as (go:index rest i) go:int64))
	    (go:when (go:> it rv) (go:= rv it)))
    rv)

  (define (min . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:return (go:as (go:index rest 0) go:int64))))
    ;; iterate
    (go::= rv (go:as (go:index rest 0) go:int64))
    (go:for (go::= i 1) (go:< i (go:len rest)) (go:++ i)
	    (go::= it (go:as (go:index rest i) go:int64))
	    (go:when (go:< it rv) (go:= rv it)))
    rv)

  (define (negative? a)
    (go:< (go:as a go:int64) 0))

  (define (number? a)
    #t)

  (define (odd? a)
    (go:!= (go:% (go:as a go:int64) 2) 0))

  (define (positive? a)
    (go:> (go:as a go:int64) 0))

  (define (rational? a)
    #t)

  (define (real? a)
    #t)

  (define (reverse-bit-field)
    #f)

  (define (rotate-bit-field)
    #f)

  ;; primitives for r7rs:truncate/
  (define (rtz/ a b)
    (go:/ (go:as a go:int64)
	  (go:as b go:int64)))

  (define (rtz% a b)
    (go:% (go:as a go:int64)
	  (go:as b go:int64)))

  ;; primitives for r7rs:floor/
  (define (rtn/ a b)
    (rtz/ (- a (rtn% a b)) b))

  (define (rtn% a b)
    (go:cond!
     ((go:== (go:as (rtz% a b) go:int64) 0)
      (go:break))
     ((go:as (negative? (* a b)) go:bool)
      (go:return (go:+ (go:as (rtz% a b) go:int64)
                       (go:as b go:int64)))))
    (rtz% a b))

  ;; primitives for r7rs:ceiling/
  (define (rtp/ a b)
    (rtz/ (- a (rtp% a b)) b))

  (define (rtp% a b)
    (rtn% a (- b)))

  ;; useless
  (define (rta/ a b)
    (rtz/ (- a (rta% a b)) b))

  ;; useless
  (define (rta% a b)
    (go:cond!
     ((go:== (go:as (rtz% a b) go:int64) 0)
      (go:break))
     ((go:as (positive? (* a b)) go:bool)
      (go:return (go:- (go:as (rtz% a b) go:int64)
                       (go:as b go:int64))))
     ((go:as (negative? (* a b)) go:bool)
      (go:return (go:+ (go:as (rtz% a b) go:int64)
                       (go:as b go:int64)))))
    (rtz% a b))

  ;; useless
  (define (rnz/ a b)
    (rtz/ (- a (rnz% a b)) b))

  (define (rnz% a b)
    (go:var (go:= #(two go:int64) 2))
    (go:cond!
     ((go:as (zero? (rtz% a b)) go:bool)
      (go:break))
     ((go:as (= (rtz% (* two a) (* two b)) b) go:bool)
      (go:break))
     ((go:as (= (rta% (* two a) (* two b)) b) go:bool)
      (go:break))
     ((go:as (> (rtn% (* two a) (* two b)) b) go:bool)
      (go:when (go:as (positive? a) go:bool)
               (go:cond!
                ((go:as (positive? b) go:bool)
                 (go:return (- (rtz% a b) b)))
                (go:else
                 (go:return (+ (rtz% a b) b)))))
      (go:break))
     ((go:as (< (rtn% (* two a) (* two b)) b) go:bool)
      (go:when (go:as (negative? a) go:bool)
               (go:cond!
                ((go:as (negative? b) go:bool)
                 (go:return (- (rtz% a b) b)))
                (go:else
                 (go:return (+ (rtz% a b) b)))))
      (go:break)))
    (rtz% a b))

  ;; primitives for r7rs:centered/, r6rs:div0-and-mod0
  (define (rnn/ a b)
    (go:var (go:= #(two go:int64) 2))
    (go:cond!
     ((go:as (zero? (rtz% a b)) go:bool)
      (go:break))
     ((go:as (= (rtz% (* two a) (* two b)) b) go:bool)
      (go:return (rtn/ a b)))
     ((go:as (= (rta% (* two a) (* two b)) b) go:bool)
      (go:return (rtn/ a b))))
    (rnz/ a b))

  (define (rnn% a b)
    (- a (* b (rnn/ a b))))

  ;; useless
  (define (rnp/ a b)
    (go:var (go:= #(two go:int64) 2))
    (go:cond!
     ((go:as (zero? (rtz% a b)) go:bool)
      (go:break))
     ((go:as (= (rtz% (* two a) (* two b)) b) go:bool)
      (go:return (rtp/ a b)))
     ((go:as (= (rta% (* two a) (* two b)) b) go:bool)
      (go:return (rtp/ a b))))
    (rnz/ a b))

  (define (rnp% a b)
    (- a (* b (rnp/ a b))))

  ;; useless
  (define (rna/ a b)
    (go:var (go:= #(two go:int64) 2))
    (go:cond!
     ((go:as (zero? (rtz% a b)) go:bool)
      (go:break))
     ((go:as (= (rtz% (* two a) (* two b)) b) go:bool)
      (go:return (rta/ a b)))
     ((go:as (= (rta% (* two a) (* two b)) b) go:bool)
      (go:return (rta/ a b))))
    (rnz/ a b))

  (define (rna% a b)
    (- a (* b (rna/ a b))))

  ;; primitives for r7rs:round/
  (define (rte/ a b)
    (define (force-rte/ a b)
      (* (go:int64 2) (rtn/ (+ (rnn/ a b) (go:int64 1)) (go:int64 2))))
    (go:var (go:= #(two go:int64) 2))
    (go:cond!
     ((go:as (zero? (rtz% a b)) go:bool)
      (go:break))
     ((go:as (= (rtz% (* two a) (* two b)) b) go:bool)
      (go:return (force-rte/ a b)))
     ((go:as (= (rta% (* two a) (* two b)) b) go:bool)
      (go:return (force-rte/ a b))))
    (rnz/ a b))

  (define (rte% a b)
    (go:% (go:as a go:int64)
	  (go:as b go:int64)))

  (define (shift)
    #f)

  (define (shift-left)
    #f)

  (define (shift-right)
    #f)

  (define (width)
    (go:int64 64))

  (define (zero? a)
    (go:== (go:as a go:int64) 0))

 )
)
