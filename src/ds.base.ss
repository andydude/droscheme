#|
 | Droscheme - a Scheme implementation
 | Copyright © 2012 Andrew Robbins, Daniel Connelly
 |
 | This program is free software: it is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 | FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
 | terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
 |#

;(define-library (ds base)
;  (export
;    *
;    +
;    -
;    /
;    abs
;    append
;    apply
;    assoc
;    assq
;    assv
;    bytevector-copy
;    bytevector-copy!
;    bytevector-copy-partial
;    bytevector-copy-partial!
;    bytevector-length
;    bytevector-u8-ref
;    bytevector-u8-set!
;    bytevector?
;    call-with-current-continuation
;    call-with-port
;    call-with-values
;    call/cc
;    car
;    cdr
;    ceiling
;    char->integer
;    char-ready?
;    char<=?
;    char<?
;    char=?
;    char>=?
;    char>?
;    char?
;    close-input-port
;    close-output-port
;    close-port
;    complex?
;    cons
;    current-error-port
;    current-input-port
;    current-output-port
;    denominator
;    dynamic-wind
;    eof-object?
;    eq?
;    equal?
;    eqv?
;    error
;    error-object-irritants
;    error-object-message
;    error-object?
;    even?
;    exact->inexact
;    exact-integer-sqrt
;    exact-integer?
;    exact?
;    expt
;    floor
;    flush-output-port
;    for-each
;    gcd
;    get-output-bytevector
;    get-output-string
;    inexact->exact
;    inexact?
;    input-port?
;    integer->char
;    integer?
;    lcm
;    length
;    list
;    list->string
;    list->vector
;    list-copy
;    list-ref
;    list-set!
;    list-tail
;    list?
;    make-bytevector
;    make-list
;    make-parameter
;    make-string
;    make-vector
;    map
;    max
;    member
;    memq
;    memv
;    min
;    modulo
;    negative?
;    newline
;    not
;    null?
;    number->string
;    number?
;    numerator
;    odd?
;    open-input-bytevector
;    open-input-string
;    open-output-bytevector
;    open-output-string
;    output-port?
;    pair?
;    peek-char
;    peek-u8
;    port-open?
;    port?
;    positive?
;    procedure?
;    quotient
;    raise
;    raise-continuable
;    rational?
;    rationalize
;    read-bytevector
;    read-bytevector!
;    read-char
;    read-line
;    read-u8
;    real?
;    remainder
;    reverse
;    round
;    set-car!
;    set-cdr!
;    string
;    string->list
;    string->number
;    string->symbol
;    string->utf8
;    string->vector
;    string-append
;    string-copy
;    string-fill!
;    string-for-each
;    string-length
;    string-map
;    string-ref
;    string-set!
;    string<=?
;    string<?
;    string=?
;    string>=?
;    string>?
;    string?
;    substring
;    symbol->string
;    symbol?
;    textual-port?
;    truncate
;    u8-ready?
;    utf8->string
;    values
;    vector
;    vector->list
;    vector->string
;    vector-copy
;    vector-fill!
;    vector-for-each
;    vector-length
;    vector-map
;    vector-ref
;    vector-set
;    vector?
;    with-exception-handler
;    write-bytevector
;    write-char
;    write-partial-bytevector
;    write-u8
;    zero?)
;  (import)
;  (begin

;; we don't have full libraries yet, so we're (load)ing this file instead

(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else => result))
     (result key))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (let ((x test1))
       (if (not x) x (and test2 ...))))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...)))))

;(define-syntax let
;  (syntax-rules ()
;    ((let ((name val) ...) body1 body2 ...)
;     ((lambda (name ...) body1 body2 ...)
;      val ...))
;    ((let tag ((name val) ...) body1 body2 ...)
;     ((letrec ((tag (lambda (name ...)
;                      body1 body2 ...)))
;        tag)
;      val ...))))
;
;(define-syntax let*
;  (syntax-rules ()
;    ((let* () body1
;     (let () body1 body2 ...))
;    ((let* ((name1 val1) (name2 val2) ...)
;       body1 body2 ...)
;     (let ((name1 val1))
;       (let* ((name2 val2) ...)
;         body1 body2 ...)))))

;(define-syntax letrec
;  (syntax-rules ()
;    ((letrec ((var1 init1) ...) body ...)
;     (letrec "generate temp names"
;       (var1 ...)
;       ()
;       ((var1 init1) ...)
;       body ...))
;    ((letrec "generate temp names" ()
;             (temp1 ...)
;             ((var1 init1) ...)
;             body ...)
;     (let ((var1 <undefined>) ...)
;       (let ((temp1 init1) ...)
;         (set! var1 temp1)
;         ...
;         body ...)))
;    ((letrec "generate temp names" (x y ...)
;             (temp ...)
;             ((var1 init1) ...)
;             body ...)
;     (letrec "generate temp names" (y ...)
;             (newtemp temp ...)
;             ((var1 init1) ...)
;             body ...))))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var1 init1) ...) body1 body2 ...)
     (let ((var1 <undefined>) ...)
       (set! var1 init1) ...
       (let () body1 body2 ...)))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values (binding ...) body0 body1 ...)
     (let-values "bind" (binding ...) () (begin body0 body1 ...)))
    ((let-values "bind" () tmps body)
     (let tmps body))
    ((let-values "bind" ((b0 e0) binding ...) tmps body)
     (let-values "mktmp" b0 e0 () (binding ...) tmps body))
    ((let-values "mktmp" () e0 args bindings tmps body)
     (call-with-values (lambda () e0)
       (lambda args (let-values "bind" bindings tmps body))))
    ((let-values "mktmp" (a . b) e0 (arg ...) bindings (tmp ...) body)
     (let-values "mktmp" b e0 (arg ... x) bindings (tmp ... (a x)) body))
    ((let-values "mktmp" a e0 (arg ...) bindings (tmp ...) body)
     (call-with-values (lambda () e0)
       (lambda (arg ... . x)
         (let-values "bind" bindings (tmp ... (a x)) body))))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body0 body1 ...)
     (begin body0 body1 ...))
    ((let*-values (binding0 binding1 ...)
       body0 body1 ...)
     (let-values (binding0)
       (let*-values (binding1 ...)
         body0 body1 ...)))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
       command ...)
     (let loop ((var init) ...)
       (when test expr ...)
       command ...
       (loop (do "step" var step ...) ...)))
    ((do "step" x) x)
    ((do "step" x y) y)))

(define-syntax lazy
  (syntax-rules ()
    ((lazy expression)
     (make-promise #f (lambda () expression)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (lazy (make-promise #t expression)))))

;(define-syntax case-lambda
;  (syntax-rules ()
;    ((case-lambda (params body0 body1 ...) ...)
;     (lambda args
;       (let ((len (length args)))
;         (let-syntax
;             ((cl (syntax-rules ::: ()
;                    ((cl)
;                     (error "no matching clause"))
;                    ((cl (if
;                          ((cl (if
;                                ((p :::) . body) . rest)
;                               (= len (length ’(p :::)))
;                               (apply (lambda (p :::)
;                                        . body) args)
;                               (cl . rest)))
;                          ((p ::: . tail) . body)
;                          . rest)
;                         (>= len (length ’(p :::)))
;                         (apply
;                          (lambda (p ::: . tail)
;                            . body)
;                          args)
;                         (cl . rest))))))
;           (cl (params body0 body1 ...) ...)))))))

;; procedures

(define *
  (lambda rest
    (fold-right num* 1 rest)))

(define +
  (lambda rest
    (fold-right num+ 0 rest)))

(define -
  (lambda rest
    (if (null? rest) 0
        (let ((n (car rest))
              (ns (cdr rest)))
          (if (null? ns) (num- 0 n)
              (num- n (apply + ns)))))))

(define /
  (lambda rest
    (if (null? rest) 1
        (let ((n (car rest))
              (ns (cdr rest)))
          (if (null? ns) (num/ 1 n)
              (num/ n (apply * ns)))))))

(define =
  (lambda rest
    (andmap num=  (most rest) (cdr rest))))
(define <
  (lambda rest
    (andmap num<  (most rest) (cdr rest))))
(define <= 
  (lambda rest 
    (andmap num<= (most rest) (cdr rest))))
(define >  
  (lambda rest 
    (andmap num>  (most rest) (cdr rest))))
(define >= 
  (lambda rest 
    (andmap num>= (most rest) (cdr rest))))

(define (abs x)
  (if (negative? x) (- x) x))

(define (append a . rest)
    (if (null? rest) a
        (fold-right cons (apply append rest) a)))

;(define (apply proc . rest)
;  ((lambda (args) (proc . args)) (cons* . rest)))

(define (assoc a as)
  (assp (lambda (x) (equal? a x)) as))

(define (assq a as)
  (assp (lambda (x) (eq? a x)) as))

(define (assv a as)
  (assp (lambda (x) (eqv? a x)) as))

(define (assp proc as)
  (if (null? as) #f
      (if (proc (caar as)) (car as)
          (assp proc (cdr as)))))

;(define (bytevector-copy))          ; core
;(define (bytevector-copy!))         ; core
;(define (bytevector-copy-partial))  ; core
;(define (bytevector-copy-partial!)) ; core
;(define (bytevector-length))        ; core
;(define (bytevector-u8-ref))        ; core
;(define (bytevector-u8-set!))       ; core
;(define (bytevector?))              ; core
;(define (call-with-port))
;(define (call-with-values))

(define call/cc call-with-current-continuation)
(define call/ec call-with-escape-continuation)
;(define call/dc call-with-composable-continuation)
;(define call/cp call-with-continuation-prompt)
;(define call/ch call-with-continuation-channel)


(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (caaar x) (car (cdr (cdr x))))
(define (caadr x) (car (cdr (cdr x))))
(define (cadar x) (car (cdr (cdr x))))
(define (caddr x) (car (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))


(define (ceiling x)
  (- (floor (- x))))

(define (ceiling/ a b)
  (let ((q (ceiling-quotient a b)))
    (values q (- a (* b q)))))

(define (ceiling-quotient a b)
  (ceiling (/ a b)))

(define (ceiling-remainder a b)
  (- a (* b (ceiling-quotient a b))))

(define (centered x)
  (floor (+ x 1/2)))

(define (centered/ a b)
  (let ((q (centered-quotient a b)))
    (values q (- a (* b q)))))

(define (centered-quotient a b)
  (centered (/ a b)))

(define (centered-remainder a b)
  (- a (* b (centered-quotient a b))))

;(define (char->integer)) ; core
;(define (char-ready?))   ; core

(define char<=? 
  (lambda rest 
    (apply <= (map char->integer rest))))
(define char<?  
  (lambda rest 
    (apply <  (map char->integer rest))))
(define char=?  
  (lambda rest 
    (apply =  (map char->integer rest))))
(define char>=? 
  (lambda rest 
    (apply >= (map char->integer rest))))
(define char>?  
  (lambda rest 
    (apply >  (map char->integer rest))))

;(define (char?)) ; core

;(define (complex?))

;(define command-line (make-parameter '())) ; core

;(define (cons* . rest)
;  (append (most rest) (last rest)))

(define (div a b)
  (euclidean-quotient a b))

(define (div0 a b)
  (centered-quotient a b))

;(define (denominator))
;(define (dynamic-wind))
;(define (eof-object?))

;(define (inexact=?)) ; core
;(define (pointer=?)) ; core
;(define (type=?)) ; core
;(define (empty?)) ; core

(define (eq? a b) (eqv? a b))
(define (eqv? a b)
  (cond
    ((not (type=? a b)) #f)
    ((and (symbol? a) (symbol? b)) (symbol=? a b))
    ((and (boolean? a) (boolean? b)) (boolean=? a b))
    ((and (inexact? a) (inexact? b)) (inexact=? a b))
    ((and (exact? a) (exact? b)) (= a b))
    ((and (char? a) (char? b)) (char=? a b))
    ((and (null? a) (null? b)) #t)
    ((and (empty? a) (empty? b)) #t)
    (else (pointer=? a b))))

;(define (error))
;(define (error-object-irritants))
;(define (error-object-message))
;(define (error-object?))

(define (euclidean/ a b)
  (let ((q (euclidean-quotient a b)))
    (values q (- a (* b q)))))

(define (euclidean-quotient a b)
  (* (sign b) (floor (/ a (abs b)))))

(define (euclidean-remainder a b)
  (- a (* b (euclidean-quotient a b))))

(define (even? n)
  (and (integer? n)
       (zero? (modulo n 2))))

;(define (exact->inexact))

(define (exact-integer-sqrt k)
  (if (not (exact-integer? k))
      (error "exact-integer-sqrt expected exact-integer" k)
      (euclidean/ (rationalize (sqrt (exact->inexact k))) 1)))

(define (exact-integer? x)
  (and (exact? x)
       (integer? x)))

;(define (exact?)) ; core
;(define (floor)) ; core

(define (floor/ a b)
  (let ((q (floor-quotient a b)))
    (values q (- a (* b q)))))

(define (floor-quotient a b)
  (floor (/ a b)))

(define (floor-remainder a b)
  (- a (* b (floor-quotient a b))))

;(define fold-left
;  (case-lambda
;    ((proc nil ls)
;     (if (null? ls) nil
;         (fold-left proc (proc nil (car ls)) (cdr ls))))
;    ((proc nil . lss)
;     (if (exists null? lss) nil
;         (let ((cars (map car lss))
;               (cdrs (map cdr lss)))
;           (apply fold-left proc (apply proc
;             (append (list nil) cars)) cdrs))))))
;
;(define fold-right
;  (case-lambda
;    ((proc nil ls)
;     (if (null? ls) nil
;         (proc (car ls) (fold-right proc nil (cdr ls)))))
;    ((proc nil . lss)
;     (if (exists null? lss) nil
;         (let ((cars (map car lss))
;               (cdrs (map cdr lss)))
;           (apply proc (append cars (list
;             (apply fold-right proc nil cdrs)))))))))

(define (exists-1 pred ls)
  (if (null? ls) #f
      (for-or (pred (car ls))
              (exists-1 pred (cdr ls)))))

(define (exists pred . rest)
  (if (exists-1 null? rest) #f
      (for-or (apply pred (map car rest))
              (apply exists pred (map cdr rest)))))

(define (for-all-1 pred ls)
  (if (null? ls) #t
      (for-and (pred (car ls))
               (for-all-1 pred (cdr ls)))))

(define (for-all pred . rest)
  (if (exists-1 null? rest) #t
      (for-and (apply pred (map car rest))
               (apply for-all pred (map cdr rest)))))

(define (for-and . ls)
  (if (null? ls) #t
      (if (null? (cdr ls))
          (if (car ls) #t #f)
          (if (car ls) (apply for-and (cdr ls)) #f))))

(define (for-or . ls)
  (if (null? ls) #f
      (if (null? (cdr ls))
          (if (car ls) #t #f)
          (if (car ls) #t (apply for-or (cdr ls))))))

; this is inefficient
(define (for-each . rest)
  (begin (map . rest) (void)))

; verbatim from R7RS
(define (force promise)
  (if (promise-done? promise)
      (promise-value promise)
      (let ((promise* ((promise-value promise))))
        (unless (promise-done? promise)
                (promise-update! promise* promise))
        (force promise))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (euclidean-remainder a b))))

(define (identity x) x)
;(define (inexact->exact))
;(define (input-port?)) ; core
;(define (integer->char)) ; core

(define (iota count . rest)
  (define (iota-3 count start step)
    (if (zero? count) '()
        (cons start (iota-3 (- count 1) (+ start step) step))))
  (let ((start (list-ref rest 0 0))
        (step  (list-ref rest 1 1)))
    (iota-3 count start step)))

(define (lcm a b)
  (/ (abs (* a b)) (gcd a b)))

(define (last ls)
  (if (null? ls) ls
      (if (null? (cdr ls)) (car ls)
          (last (cdr ls)))))

;(define (list* . rest) ; core
;  (apply apply list rest))
;
;(define (list+ a . rest) ; core
;  (append a rest))

;(define (list->string))
;(define (list->vector))
;(define (list-copy))
;(define (list-ref))
;(define (list-set!))
;(define (list-tail))
;(define (make-bytevector))
;(define (make-list))
;(define (make-parameter))
;(define (make-string k))
;(define (make-vector))

(define make-promise
  (lambda (done? proc)
    (list (cons done? proc))))

(define (make-eqv-hashtable . rest)
  (apply make-hashtable hash eqv? rest))

(define (make-eq-hashtable . rest)
  (apply make-hashtable hash eq? rest))

(define (map-1 proc ls)
  (if (null? ls) ()
      (cons (proc (car ls))
            (map-1 proc (cdr ls)))))

(define (map proc . rest)
  (if (equal? (length rest) 1) (map-1 proc (car rest))
      (if (exists-1 null? rest) ()
          (let ((cars (map-1 car rest))
                (cdrs (map-1 cdr rest)))
            (cons (apply proc cars)
                  (apply map proc cdrs))))))

; TODO: alphabetize these

(define (andmap proc . rest)
  (apply for-and (apply map proc rest)))

(define (ormap proc . rest)
  (apply for-or (apply map proc rest)))


(define (max a . rest)
  (if (null? rest) a
      (let ((b (apply max rest)))
        (if (> a b) a b))))

(define (memp proc ls)
  (if (null? ls) #f
      (if (proc (car ls)) ls
          (memp proc (cdr ls)))))

(define (member a ls)
  (memp (lambda (x) (equal? a x)) ls))

(define (memq a ls)
  (memp (lambda (x) (eq? a x)) ls))

(define (memv a ls)
  (memp (lambda (x) (eqv? a x)) ls))

(define (min a . rest)
  (if (null? rest) a
      (let ((b (apply min rest)))
        (if (< a b) a b))))

(define (mod a b)
  (euclidean-remainder a b))

(define (mod0 a b)
  (centered-remainder a b))

(define (modulo a b)
  (floor-remainder a b))

(define (most ls)
  (if (null? ls) ()
      (if (null? (cdr ls)) ()
          (cons (car ls) (most (cdr ls))))))

(define (negative? x)
  (if (real? x)
      (num< x 0)
      #f))

(define (newline . opt)
  (apply write-char #\newline opt))

;(define (not))
;(define (number->string))
;(define (number?))
;(define (numerator))

(define (odd? n)
  (and (integer? n)
       (not (zero? (modulo n 2)))))

;(define (open-input-bytevector))
;(define (open-input-string))
;(define (open-output-bytevector))
;(define (open-output-string))
;(define (output-port?))
;(define (peek-char))
;(define (peek-u8))
;(define (port-open?))
;(define (port?))

(define (positive? x)
  (if (real? x)
      (num> x 0)
      #f))

;(define (procedure?))

(define (quotient a b)
  (truncate-quotient a b))

;(define (raise))
;(define (raise-continuable))
;(define (rational?))
;(define (rationalize))

(define (rationalize x e)
  (if (or (inexact? x)
          (inexact? e))
      (exact->inexact (simplest-rational (- x e) (+ x e)))
      (simplest-rational->exact (- x e) (+ x e))))

(define (rationalize->exact x e)
  (rationalize (inexact->exact x) (inexact->exact e)))

;(define (read-bytevector))
;(define (read-bytevector!))
;(define (read-char))
;(define (read-line))
;(define (read-u8))
;(define (real?))

(define (real-zero? x)
  (equal? x 0))

(define (remp proc ls)
  (if (null? ls) ()
      (if (proc (car ls))
          (remp proc (cdr ls))
          (cons (car ls) (remp proc (cdr ls))))))

(define (remove a ls)
  (remp (lambda (x) (equal? a x)) ls))

(define (remq a ls)
  (remp (lambda (x) (eq? a x)) ls))

(define (remv a ls)
  (remp (lambda (x) (eqv? a x)) ls))

(define (remainder a b)
  (truncate-remainder a b))

(define (reverse ls)
  (if (null? ls) ls
      (append (reverse (cdr ls)) (list (car ls)))))

(define (round/ a b)
  (let ((q (round-quotient a b)))
    (values q (- a (* b q)))))

(define (round-quotient a b)
  (round (/ a b)))

(define (round-remainder a b)
  (- a (* b (round-quotient a b))))

;(define (round)) ; core

;; from <trac.sacrideo.us/wg/wiki/RationalizeDefinition>
(define (simplest-rational x y)
  (define (simplest-rational-internal x y)
    ;; assumes 0 < X < Y
    (let ((fx (floor x))
          (fy (floor y)))
      (cond ((not (< fx x))
             fx)
            ((= fx fy)
             (+ fx
                (/ (simplest-rational-internal
                    (/ (- y fy))
                    (/ (- x fx))))))
            (else
             (+ 1 fx)))))
  ;; do some juggling to satisfy preconditions
  ;; of simplest-rational-internal.
  (cond ((< y x)
         (simplest-rational y x))
        ((not (< x y))
         (if (rational? x) x (error)))
        ((positive? x)
         (simplest-rational-internal x y))
        ((negative? y)
         (- (simplest-rational-internal (- y)
                                        (- x))))
        (else 0)))

(define (simplest-rational->exact x y)
  (simplest-rational (inexact->exact x) (inexact->exact y)))

;(define (string->list))
;(define (string->number))
;(define (string->symbol))
;(define (string->utf8))
;(define (string->vector))
;(define (string-append))
;(define (string-copy))
;(define (string-fill!))

(define (string-for-each proc s)
  (for-each proc (string->list s)))

;(define (string-length s)
;  (length (string->list s)))

(define (string-map proc s)
  (list->string (map proc (string->list s))))

;(define (string-ref))  ; core
;(define (string-set!)) ; core

(define string<=? 
  (lambda rest 
    (apply andmap char<=? (map string->list rest))))
(define string<?  
  (lambda rest 
    (apply andmap char<?  (map string->list rest))))
(define string=?  
  (lambda rest 
    (apply andmap char=?  (map string->list rest))))
(define string>=? 
  (lambda rest 
    (apply andmap char>=? (map string->list rest))))
(define string>?  
  (lambda rest 
    (apply andmap char>?  (map string->list rest))))

;(define (string?))

;(define (substring)) ; core

;(define (symbol->string))
;(define (textual-port?))

(define (symbol=? a b)
  (string=? (symbol->string a)
            (symbol->string b)))

(define (symbol-ci=? a b)
  (string-ci=? (symbol->string a)
               (symbol->string b)))

(define (truncate x)
  (* (sign x) (floor (abs x))))

(define (truncate/ a b)
  (let ((q (truncate-quotient a b)))
    (values q (- a (* b q)))))

(define (truncate-quotient a b)
  (truncate (/ a b)))

(define (truncate-remainder a b)
  (- a (* b (truncate-quotient a b))))

;(define (u8-ready?))
;(define (utf8->string))
;(define (vector->list))
;(define (vector->string))
;(define (vector-copy))
;(define (vector-fill!))

(define (vector-for-each proc v)
  (for-each proc (vector->list v)))

;(define (vector-length v)) ; core

(define (vector-map proc v)
  (list->vector (map proc (vector->list v))))

;(define (vector-ref)) ; core
;(define (vector-set!)) ; core

;(define (with-exception-handler))
;(define (write-bytevector))
;(define (write-char))

(define (write-partial-bytevector v start end . o)
  (apply write-bytevector (bytevector-copy-partial v start end) o))

;(define (write-u8))

(define (zero? z)
  (if (real? z)
      (real-zero? z)
      (if (complex? z)
          (and (real-zero? (real-part z))
               (real-zero? (imag-part z))))))

(define #%key cons)

;);begin
;);define-library