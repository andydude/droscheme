(define-library
  (ds any syntax)
  (import (dot (ds port)))
  (export
    list-match?
    list-replace
    read
    symbol-match?
    symbol-replace
    syntax-match?
    syntax-replace
    vector-match?
    vector-replace
    write)
  (begin
    (define (list-match? patt syntax env)
      (:= (cap cdp) (car+cdr patt))
      (if1 (pair? cdp)
           (:= cadp (car cdp))
           (if1 (and (as (symbol? cadp) bool)
                     (== (symbol->string cadp) "..."))
                (if1 (not (symbol? cap))
                     (error "ellipsis is only implemented for symbols")
                     (return (void)))
                (if1 (!= (as env (ptr Env) (Ref cap)) (inline "nil"))
                     (error "list-match expected unbound symbol"))
                (if1 (or (as (null? syntax) bool)
                         (as (pair? syntax) bool))
                     (return #t))
                (return #f)))
      (:= (cas cds) (car+cdr syntax))
      (if1 (not (syntax-match? cap cas env))
           (return #f))
      (if1 (not (syntax-match? cdp cds env))
           (return #f))
      #t)
    (define (list-replace temp env)
      (:= (cat cdt) (car+cdr temp))
      (if1 (pair? cdt)
           (:= cadt (car cdr))
           (if1 (and (as (symbol? cadt) bool)
                     (== (symbol->string cadt) "..."))
                (return (as env (ptr Env) (Ref cat)))))
      (:= (cas cds)
          (syntax-replace cat env)
          (syntax-replace cdt env))
      (cons cas cds))
    (define (symbol-match? patt syntax env)
      (:= name (as (->immutable-string patt) string))
      (if1 (== name "_") (return #t))
      (if1 (== name "...")
           (error "we were supposed to catch ... earlier"))
      (if2 (:= value (as env (ptr Env) (Ref patt)))
           (and (!= value (inline "nil"))
                (as (equal? patt value) bool))
           (return (equal? patt syntax)))
      (as env (ptr Env) (Define patt syntax))
      #t)
    (define (symbol-replace temp env)
      (:= value (as env (ptr Env) (Ref temp)))
      (if1 (== value (inline "nil")) (return temp))
      value)
    (define (syntax-match? patt syntax env)
      (if1 (list? patt)
           (return (list-match? patt syntax env)))
      (if1 (symbol? patt)
           (return (symbol-match? patt syntax env)))
      (equal? patt syntax))
    (define (syntax-replace temp env)
      (if1 (list? temp)
           (return (list-replace temp env)))
      (if1 (symbol? temp)
           (return (symbol-replace temp env)))
      temp)
    (define (read #(port (current-input-port)))
      (:= (value err) (Read (as port TIPort)))
      (if1 (!= err (inline "nil"))
           (call panic (->immutable-string err)))
      value)
    (define (write obj #(port (current-output-port)))
      (:= rep (->scheme-string obj))
      (as port
          BOPort
          (Write ((array-slice byte) (as rep string))))
      (void))))