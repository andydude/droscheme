(define-library
  (ds any env null)
  (export null-environment)
  (begin
    (define (null-environment version)
      (:= env (NewEnv))
      (switch
        (as version int)
        (case #\D
          (dot env (Add apply-syntax))
          (dot env (Add current-environment))
          (dot env (Add define-macro))
          (dot env (Add eval-syntax))
          (dot env (Add load-syntax))
          (fallthrough))
        (case 7
          (dot env (Add define-library))
          (dot env (Add define-record-type))
          (dot env (Add define-values))
          (dot env (Add guard))
          (dot env (Add parameterize))
          (fallthrough))
        (case 6
          (dot env (Add assert))
          (dot env (Add case-lambda))
          (dot env (Add identifier-syntax))
          (dot env (Add library))
          (dot env (Add quasisyntax))
          (dot env (Add syntax))
          (dot env (Add syntax-case))
          (fallthrough))
        (case 5
          (dot env (Add define-syntax))
          (dot env (Add let-syntax))
          (dot env (Add letrec-syntax))
          (dot env (Add syntax-rules)))
        (case 4
          (dot env (Add begin))
          (dot env (Add case))
          (dot env (Add cond))
          (dot env (Add define))
          (dot env (Add do))
          (dot env (Add if))
          (dot env (Add lambda))
          (dot env (Add let))
          (dot env (Add let*))
          (dot env (Add letrec))
          (dot env (Add load))
          (dot env (Add quasiquote))
          (dot env (Add quote))
          (dot env (Add set!))
          (dot env (Add unquote))
          (dot env (Add unquote-splicing))))
      env)))
