(define-library
  (ds base env)
  (export
    interaction-environment
    scheme-base-environment
    scheme-report-environment)
  (import
    (ds any env null)
    (ds any env droscheme)
    (ds any env scheme-base-7)
    (ds any env scheme-report-7)
    (ds any runtime))
  (begin
    (define (null-environment)
      (environment '(ds any env null)))
    (define (interaction-environment)
      (environment '(ds any env droscheme)))
    (define (scheme-base-environment version)
      (case! (as version int)
             ((7)
              (return
                (environment '(ds any env scheme-base-7)))))
      (NewEnv))
    (define (scheme-report-environment version)
      (case! (as version int)
             ((7)
              (return
                (environment '(ds any env scheme-report-7)))))
      (NewEnv))))