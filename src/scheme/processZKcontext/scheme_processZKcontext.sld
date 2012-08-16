(define-library
  (scheme process-context)
  (import (ds base) "os")
  (export
    command-line
    exit
    get-environment-variable
    get-environment-variables)
  (begin
    (define (command-line-vector)
      (Vector (dot os Args)))
    (define (command-line)
      (vector->list (command-line-vector)))
    (define (exit #(obj #t))
      (:= code (if obj 0 1))
      (dot os (Exit code))
      (void))
    (define (get-environment-variable name)
      (let* ((key (string->immutable-string name)))
        (immutable-string->string (dot os (Getenv key)))))
    (define (get-environment-variables-vector)
      (:= env (dot os (Environ)))
      (:= ret (as (make-vector (call len env)) Vector))
      (range (:= (k pair) env)
             (:= str (immutable-string->string pair))
             (:= idx (string-index str #\=))
             (:= key (substring str 0 idx))
             (:= val (substring str (+ idx 1)))
             (dot ret (Set k (cons key val))))
      ret)
    (define (get-environment-variables)
      (vector->list (get-environment-variables-vector)))))