(define-library main
(import
 (prefix (gos) go:)
 (ds any)
 (ds any port)
 (ds scheme base)
 (ds scheme runtime)
 (ds scheme syntax)
 (runtime debug))

(go:var
 (go:= input (standard-input-port))
 (go:= output (standard-output-port))
 (go:= env (interaction-environment)))

(go:while
 #t
 (go:defer
  ((go:func
    ()
    go:&void
    (go::= err (go:recover))
    (go:when
     (go:!= err go:nil)
     (debug.PrintStack)
     (go:panic err)))))
 (go:print "\n> ")
 (go::= exp (read-lines input))
 (go::= value (eval exp env))
 (write value output))

);define-library
