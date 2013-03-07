(define-library
 (ds scheme write)
 (import
  (only (ds any) ->scheme-string))
 (export write)
 (begin

  (define (display str)
    (go:print str)
    go:nil)

  (define (write obj (port (current-output-port)))
    (display (->scheme-string obj)))
    
 )
)
