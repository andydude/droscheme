; Drosera Function Class
;
; The methods are implemented as functions,
; where the first argument is a Function object.


; Dictionary keys:
;
;   signature            ((x Any) Any)
;   go-signature         "(x Any) Any"
;   go-body              "{ return x; }"
;   go-name              "_condZKexpand"
;   go-recv
;   mangled-name         "condZKexpand"
;   library-name         (scheme cond-expand)
;   mangled-library-path "scheme/condZKexpand"
;   mangled-library-name "scheme_condZKexpand"
;   go-library-name     "_scheme_condZKexpand"
;   has-return? (if #f then add "return " to last line)
;   is-imported?
;   is-method?
;
;TODO:
; (ds-function-input-name proc k)
; (ds-function-input-names proc)
; (ds-function-input-type proc k)
; (ds-function-input-types proc)
; (ds-function-output-name proc)
; (ds-function-output-names proc)
; (ds-function-output-type proc)
; (ds-function-output-types proc)
;

(define (make-ds-function name formals . dictionary)
  `#(droscheme-function ,name ,formals
    ,(list->eqv-hashtable dictionary)))

(define (ds-function? proc)
  (eqv? (vector-ref proc 0) 'droscheme-function))

(define (ds-function-body proc)
  (ds-function-attributes-ref proc 'begin))

(define (ds-function-name-public? name)
  (if (symbol? name)
      (ds-function-name-public? (symbol->string name))
      (if (and (string? name) (<= 1 (string-length name)))
          (char-upper-case? (string-ref name 0))
          #t)))

(define (ds-function-name proc)
  (vector-ref proc 1))

(define (ds-function-signature proc)
  (vector-ref proc 2))

(define (ds-function-attributes proc)
  (vector-ref proc 3))

(define (ds-function-attributes-has? proc key)
  (hashtable-contains? (ds-function-attributes proc) key))

(define (ds-function-attributes-ref proc key)
  (hashtable-ref (ds-function-attributes proc) key))

(define (ds-function-attributes-set! proc key value)
  (vector-set! proc 3 (hashtable-set! (vector-ref proc 3) key value))
  proc)

(define (ds-function-imported? proc)
  (equal? (ds-function-attributes-ref proc 'library-name)
          (current-library-name)))

(define (ds-function-library-name proc)
  ;(write (ds-function-attributes-has? proc 'library-name))
  (if (ds-function-attributes-has? proc 'library-name)
      (ds-function-attributes-ref proc 'library-name)
      (current-library-name)))

(define (ds-function-mangled-library-name proc)
  (if (ds-function-attributes-has? proc 'mangled-library-name)
      (ds-function-attributes-ref proc 'mangled-library-name)
      (list->underscore (ds-function-library-name proc))))

(define (ds-function-mangled-name proc)
  (if (ds-function-attributes-has? proc 'mangled-name)
      (ds-function-attributes-ref proc 'mangled-name)
      (let ((mangled (symbol->mangle (ds-function-name proc))))
        (ds-function-attributes-set! proc 'mangled-name mangled)
        mangled)))

(define (ds-function-method? proc)
  (ds-function-attributes-has? proc 'recv))

;; compiler logic

(define (ds-function->go-name proc)
  (if (ds-function-attributes-has? proc 'go-name)
      (ds-function-attributes-ref proc 'go-name)
      (string-append (ds-function-mangled-name proc))))

(define (ds-function->go-body proc)
  (let* ((stmts (map emit (ds-function-body proc)))
         (block (string-join stmts "\n\t")))
    (string-append "{\n\t" block "\n}\n")))

(define (ds-function->go-func proc)
  (if (ds-function-method? proc)
      (ds-function->go-method-stmt proc)
      (if (ds-function-name proc)
          (ds-function->go-func-decl proc)
          (ds-function->go-func-expr proc))))

(define (ds-function->go-func-decl proc)
  (let ((name (ds-function->go-name proc))
        (sig  (ds-function->go-signature proc))
        (body (ds-function->go-body proc)))
    (string-append "func " name sig " " body)))

(define (ds-function->go-func-expr proc)
  (let ((sig  (ds-function->go-signature proc))
        (body (ds-function->go-body proc)))
    (string-append "func" sig " " body)))

(define (ds-function->go-func-stmt proc)
  (let ((name (ds-function->go-name proc))
        (sig  (ds-function->go-signature proc))
        (body (ds-function->go-body proc)))
    (string-append "var " name " = func" sig " " body)))

(define (ds-function->go-func-type proc)
  (let ((sig  (ds-function->go-signature proc))
        (body (ds-function->go-body proc)))
    (string-append "func" sig)))

(define (ds-function->go-method-stmt proc)
  (let ((rec  (ds-function->go-receiver proc))
        (name (ds-function->go-name proc))
        (sig  (ds-function->go-signature proc))
        (body (ds-function->go-body proc)))
    (string-append "func " rec " " name sig " " body)))

(define (ds-function->go-receiver proc)
  (let* ((recv (ds-function-attributes-ref proc 'recv))
         (nm (symbol->string (vector-ref recv 0)))
         (ty (emit (vector-ref recv 1))))
    (string-append "(" nm " " ty ")")))

(define (ds-function->gos-signature proc)
  (if (ds-function-attributes-has? proc 'signature)
      (ds-function-attributes-ref proc 'signature)
      #f))

(define (ds-function->go-library-name proc)
  (if (ds-function-attributes-has? proc 'go-library-name)
      (ds-function-attributes-ref proc 'go-library-name)
      (string-append "_" (ds-function-mangled-library-name proc))))

(define (ds-function->gos-import proc)
  (let ((name  (symbol->string (ds-function-name proc)))
        (mname (string->symbol (ds-function-mangled-name proc)))
        (gname (ds-function->go-name proc))
        (lname (string->symbol (ds-function->go-library-name proc)))
        (sig   (ds-function->gos-signature proc)))
    `(= ,mname (as (index ,lname ,name) (func ,(most sig) ,(last sig))))))

(define (ds-function->go-import proc)
  (let ((sname  (ds-function-name proc))
        (mname  (ds-function-mangled-name proc))
        (name   (ds-function->go-name proc))
        (lname  (ds-function->go-library-name proc))
        (sig    (ds-function->go-signature proc)))
    (if (not proc) ""
    (string-append "var " mname " = " lname ".Ref("
                   (emit-string (symbol->string sname)) ").(Named)\n"
                   "var " name " = " mname ".Value().(func" sig ")\n"))))

(define (ds-function->gos-export proc)
  `(dot env (Add ,(ds-function-mangled-name proc))))

(define (ds-function->go-signature proc)
  (emit-lambda-signature (ds-function->gos-signature proc)))

(define (ds-function->gos-define proc)
  (let ((name (ds-function-name proc))
        (sig  (ds-function-signature proc))
        (body (ds-function-body proc)))
    `(define ,(cons name sig) ,body)))

(define (ds-function->gos-define-func proc)
  (let ((name (ds-function-name proc))
        (sig  (ds-function->gos-signature proc))
        (body (append-return (ds-function-body proc))))
    `(func ,name ,(most sig) ,(last sig) ,@body)))

(define (define->ds-function expr)
  (when (not (eqv? (car expr) 'define))
        (error "define->ds-function expected 'define, got:" expr))
  (let* ((r (cdr expr))
         (sig (car r))
         (body (cdr r))
         (name (car sig))
         (sig (cdr sig)))
    (make-ds-function name sig
      (cons 'signature (append (list->signature sig) '(&any)))
      (cons 'begin body)
      (cons 'library (current-library-name)))))

(define (define-func->ds-function expr)
  (when (not (eqv? (car expr) 'define-func))
        (error "define-func->ds-function expected 'define-func, got:" expr))
  (if (symbol? (car expr))

      ;; function definition
      (let* ((r (cdr expr))
             (sig (car r))
             (body (cdr r))
             (name (car sig))
             (sig (cdr sig))
             (args (map vector-first (most sig))))
        (make-ds-function name args
             (cons 'signature sig)
             (cons 'begin body)
             (cons 'library (current-library-name))))

      ;; method definition
      (let* ((r (cdr expr))
             (sig (car r))
             (body (cdr r))
             (recv (car sig))
             (sig (cdr sig))
             (name (car sig))
             (sig (cdr sig))
             (args (map vector-first (most sig))))
        (make-ds-function name args
             (cons 'go-name (symbol->string name))
             (cons 'recv recv)
             (cons 'signature sig)
             (cons 'begin body)
             (cons 'library (current-library-name))))))

(define (lambda->ds-function expr)
  (when (not (eqv? (car expr) 'lambda))
        (error "lambda->ds-function expected 'lambda"))
  (let* ((r (cdr expr))
         (sig (car r))
         (body (cdr r)))
    (make-ds-function "" sig
      (cons 'signature (append (list->signature sig) '(&any)))
      (cons 'begin (append-return body))
      (cons 'library (current-library-name)))))

(define (append-return body)
  (append (most body) (list (list 'return (last body)))))

(define (lambda-func->ds-function expr)
  (when (not (eqv? (car expr) 'lambda-func))
        (error "lambda-func->ds-function expected 'lambda-func"))
  (let* ((r (cdr expr))
         (sig (car r))
         (body (cdr r))
         (args (map vector-first (most sig))))
    (make-ds-function "" args
      (cons 'signature sig)
      (cons 'begin body)
      (cons 'library (current-library-name)))))

(define (ds-function->ds-library-env proc)
  (let ((name (ds-function-name proc))
        (sig  (ds-function-signature proc))
        (attr (ds-function-attributes proc)))
    (apply list name sig (hashtable->list attr))))

(define (ds-function->define proc)
  (let ((name (ds-function-name proc))
        (sig  (ds-function-signature proc))
        (body (ds-function-body proc)))
    `(define ,(cons name sig) ,@body)))

(define (ds-function->lambda proc)
  (let ((sig  (ds-function-signature proc))
        (body (ds-function-body proc)))
    `(lambda ,sig ,@body)))

;(define (ds-env->ds-function expr)
;  (apply make-ds-function expr))

;; ======================================================================
;; primary definitions
;; ======================================================================

;(define (ds-function->go)
;  (if (symbol? fsig)
;      (apply emit-define-func fsig rest)
;      (begin
;        (append-defines (car fsig))
;        (if (pair? fsig)
;            (apply emit-define-func
;                   (append (list (car fsig)) (list->signature (cdr fsig)) (list rt))
;                   (append (map emit-define-optional (iota (length (*optionals*)))
;                                (*optionals*)) (most rest)
;                                (list (cons 'return (list (last rest))))))
;            "//WHAT?"))))

;; list->signature
;;
;; This is an important function which adds
;; type information to function signatures.
;;
;; Example:
;;   scheme: (define (+ x y) ...) =>
;;   gosc: (list->signature '(x y)) =>
;;   gos: (define-func (+ (x y Any) Any) ...) =>
;;   go: func _ZI(x, y, Any) Any { ... }
(define (list->signature ls)
  ;; UPDATED
  (set-optionals '())
  (cond
   ((null? ls) '())
   ((pair? ls)
    (let ((fs (car ls))
          (rs (cdr ls)))
      (cond
       ((null? fs) (error "list->signature" ls))
       ((vector? fs) (list fs (list->signature rs)))
       ;((pair? fs)
       ; (begin (set-optionals ls)
       ;        (list '(Rest (preellipsis Any)))))
       (else (cons `#(,fs &any) (list->signature rs))))))
   (else (list `(,ls (preellipsis &any))))))

(define (list->signature-old ls)
  (define (argument-prelist ls)
    (if (null? ls)
        '()
        (if (pair? ls)
            (if (vector? (car ls))
                (begin
                  (set-optionals ls)
                  (list `(Rest (preellipsis Any))))
                (cons `(,(car ls) Any) (argument-prelist (cdr ls))))
            (list `(,ls (preellipsis Any))))))
  (set-optionals '())
  (let ((pre (argument-prelist ls)))
    ;;
    (if (and (pair? (last pre))
             (pair? (cdr (last pre)))
             (pair? (cadr (last pre))))
        (if (null? (most pre))
            (list (last pre))
            (list (append (map car (most pre)) '(Any)) (last pre) 'Boo))
        (if (null? pre) '()
            (list (append (map car pre) '(Any)))))))
