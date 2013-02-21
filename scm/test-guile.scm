;(use-modules (ice-9 syncase))
(use-modules (ice-9 pretty-print))

(define-syntax case2
  (lambda (stx)
    (syntax-case stx (case! else)
      ((_ e c1 ...)
       #'(case! e c1 ...)))))

(pretty-print
 (syntax->datum
  (macroexpand
   (datum->syntax #f
    '(case2 x
            ((y) z)
            (else a))))))
