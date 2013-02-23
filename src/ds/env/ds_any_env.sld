(define-library
  (ds any env)
  (export
    define->lambda
    environment
    environment-define
    environment-define!
    environment-define-lambda
    environment-extend
    environment-ref
    environment-set!
    environment-update)
  (import
    (ds any env null)
    (ds any env droscheme)
    (ds any env scheme-report-7)
    (ds any runtime))
  (begin
    (define (define->lambda syntax) (void))
    (define (environment . import-specs)
      (error "environment is only available at compile-time"))
    (define (environment-define env symbol value)
      (:= name (->immutable-string symbol))
      (as env (ptr Env) (Define name value))
      (void))
    (define (environment-define! env symbol value)
      (void))
    (define (environment-define-lambda env symbol value)
      (void))
    (define (environment-extend env)
      (as env (ptr Env) (Extend)))
    (define (environment-ref env symbol #(value nil))
      (:= name (->immutable-string symbol))
      (as env (ptr Env) (Refer name value)))
    (define (environment-set! env symbol value)
      (:= name (->immutable-string symbol))
      (as env (ptr Env) (Set name value))
      (void))
    (define (environment-update env)
      (as env (ptr Env) (Update)))))
