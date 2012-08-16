(define-library
  (scheme eval)
  (export
    environment
    eval
    null-environment
    scheme-report-environment)
  (import (ds base))
  (begin
    (define (environment . import-specs) (void))
    (define (eval expression env-spec)
      (__eval-syntax env-spec expression))
    (define (null-environment version)
      (switch
        version
        (case 7 (return (environment (ds env null-7))))
        (case 6 (return (environment (ds env null-6))))
        (case 5 (return (environment (ds env null-5))))
        (case 4 (return (environment (ds env null-4))))
        (case 3 (return (environment (ds env null-3))))
        (case 2 (return (environment (ds env null-2)))))
      (null-environment version))
    (define (scheme-report-environment version)
      (switch
        version
        (case 7
          (return (environment (ds env scheme-report-7))))
        (case 6
          (return (environment (ds env scheme-report-6))))
        (case 5
          (return (environment (ds env scheme-report-5))))
        (case 4
          (return (environment (ds env scheme-report-4))))
        (case 3
          (return (environment (ds env scheme-report-3))))
        (case 2
          (return (environment (ds env scheme-report-2)))))
      (null-environment version))))
