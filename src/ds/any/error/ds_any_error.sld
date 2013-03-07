(define-library
 (ds any error)
 (import)
 (export
  error
  error-object-irritants
  error-object-message
  error-object?
  make-error-object
  raise
  raise-continuable)
 (begin

  (define (make-error-object msg . irrs)
    (go:make: ErrorObject
              (go:: message (go:as msg go:string))
              (go:: irritants irrs)))

  (define (error-object? obj)
    (go::= (_ ok) (go:as obj ErrorObject))
    ok)

  (define (error-object-message obj)
    ((go:dot (go:as obj ErrorObject) Message)))

  (define (error-object-irritants obj)
    ((go:dot (go:as obj ErrorObject) Irritants)))

  (define (error msg . irrs)
    (raise (go:apply make-error-object msg irrs)))

  (define (raise obj)
    (go:panic obj)
    go:nil)

  (define (raise-continuable obj)
    (go:panic obj)
    go:nil)

 )
)
