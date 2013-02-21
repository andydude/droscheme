(require "gos/main.rkt")

;(module gos racket/base
;  (provide #%app #%top)
;  (define #%app list)
;  (define (#%top . x)
;    (string->symbol (string-append "_" (symbol->string x)))))
;
;(define (emit ob)
;  (eval ob (module->namespace 'gos)))

(write
 (parameterize ([current-namespace (module->namespace "gos/main.rkt")])
  (syntax->datum
   (expand
    (datum->syntax #f
     '(case2 x
             ((y) z)
             (else2 a)))))))
