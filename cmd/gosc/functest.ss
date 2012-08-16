(load "common.ss")

(define emit-defines emit-defines-export)
(define emit-define-func emit-define-func-compile)
(define emit-import emit-import-blank)
(define emit-package emit-package-compile)

(define *define*
  '(define (any-hash . rest)
    (if1 (== (call-go len rest) 1)
         (return (as (index rest 0) Hasher (Hash))))
    (:= hashport (inline "crc32.NewIEEE()"))
    (for (:= i 0) (< i (len v)) (++ i)
         (:= (_ err) (inline "hash32.Write([]byte(_anyZKstring(v[i])))"))
         (if1 (!= err nil)
              (call-go panic err)))
    (call-go uintptr (hash32.Sum32)))
)

(define *env* '(
 (cos (x)
      (signature (x Any) Any)
      (begin (as x TrigNum (Cos)))
      (library (scheme inexact)))
 (sin (x)
      (signature (x Any) Any)
      (begin (as x TrigNum (Sin)))
      (library (scheme inexact)))
 (any-hash rest (library (ds any)) (begin (if1 (== (call-go len rest) 1) (return (as (index rest 0) Hasher (Hash)))) (:= hashport (inline "crc32.NewIEEE()")) (for (:= i 0) (< i (len v)) (++ i) (:= (_ err) (inline "hash32.Write([]byte(_anyZKstring(v[i])))")) (if1 (!= err nil) (call-go panic err))) (call-go uintptr (hash32.Sum32))) (signature (rest (preellipsis Any))))

))

(define (emit-f item)
  (let* ((proc (apply make-ds-function item))
         (text (ds-function->go-import proc)))
    (display text)))

(map emit-f *env*)

;(write (ds-function->ds-library-env (define->ds-function *define*)))