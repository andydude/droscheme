;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Classes
;;

; (define-record-type Function
;   make-function
;   function?
;   (name)      -- PackageExport
;   (signature) -- FunctionSignature
;   (body)      -- BodyExpr
;   (meta)      -- Hashtable
; )
(define (make-ds-function export signature body . meta)
  `#(droscheme-function ,export ,signature ,body
    ,(list->eqv-hashtable meta)))

(define (ds-function? fn)
  (eqv? (vector-ref fn 0) 'droscheme-function))

(define (ds-function-export fn)
  (unless (ds-function? fn)
          (error "TypeError in ds-function-export"))
  (vector-ref fn 1))

(define (ds-function-signature fn)
  (unless (ds-function? fn)
          (error "TypeError in ds-function-signature"))
  (vector-ref fn 2))

(define (ds-function-body fn)
  (unless (ds-function? fn)
          (error "TypeError in ds-function-body"))
  (vector-ref fn 3))

(define (ds-function-meta fn)
  (unless (ds-function? fn)
          (error "TypeError in ds-function-meta"))
  (vector-ref fn 4))

(define (ds-function-meta-has? fn key)
  (hashtable-contains? (ds-function-meta fn) key))

(define (ds-function-meta-ref fn key)
  (hashtable-ref (ds-function-meta fn) key))

(define (ds-function-meta-set! fn key value)
  (let* ((meta (ds-function-meta fn))
         (hash (hashtable-set! meta key value)))
    (vector-set! fn 3 hash)
    fn))

(define (ds-function-name fn)
  (ds-package-export-name (ds-function-export fn)))


; (define-record-type FunctionSignature
;   make-function-signature
;   function-signature?
;   (self-parameters)   -- List of FunctionParameter
;   (input-parameters)  -- List of FunctionParameter
;   (output-parameters) -- List of FunctionParameter
;   (meta) -- Hashtable
; )
(define (make-ds-function-signature parameters . meta)
  `#(droscheme-function-signature ,parameters
    ,(list->eqv-hashtable meta)))

(define (ds-function-signature? fs)
  (eqv? (vector-ref fs 0) 'droscheme-function-signature))
(define (ds-function-signature-parameters fs)
  (unless (ds-function-signature? fs)
          (error "TypeError in ds-function-signature-parameters"))
  (vector-ref fs 1))
(define (ds-function-signature-meta fs)
  (unless (ds-function-signature? fs)
          (error "TypeError in ds-function-signature-meta"))
  (vector-ref fs 2))

(define (ds-function-signature-self-parameters fs)
  (filter ds-function-self-parameter?
          (ds-function-signature-parameters fs)))

(define (ds-function-signature-input-parameters fs)
  (filter ds-function-input-parameter?
          (ds-function-signature-parameters fs)))

(define (ds-function-signature-output-parameters fs)
  (filter ds-function-output-parameter?
          (ds-function-signature-parameters fs)))

(define (ds-function-signature->gos-self-parameters fs)
  (let ((self (ds-function-signature-self-parameters fs)))
    (if (null? self) '()
        (ds-function-parameter->gos-formal (car self)))))

(define (ds-function-signature->gos-input-parameters fs)
  (let ((ins (ds-function-signature-input-parameters fs)))
    (map ds-function-parameter->gos-formal ins)))

(define (ds-function-signature->gos-output-parameters fs)
  (let ((outs (ds-function-signature-output-parameters fs)))
    (if (= (length outs) 1)
        (ds-function-parameter-type (car outs))
        (map ds-function-parameter->gos-formal outs))))

(define (ds-function-signature->gos fs)
  (list (ds-function-signature->gos-input-parameters fs)
        (ds-function-signature->gos-output-parameters fs)))


; (define-record-type FunctionParameter
;   make-function-parameter
;   function-parameter?
;   (name) -- Symbol
;   (type) -- TypeExpr
;   (init) -- Expr
;   (role) -- One of '(in out self)
;   (meta) -- Hashtable
; )
(define (make-ds-function-parameter name type init role . meta)
  `#(droscheme-function-parameter ,name ,type ,init ,role
    ,(list->eqv-hashtable meta)))

(define (ds-function-parameter? fp)
  (eqv? (vector-ref fp 0) 'droscheme-function-parameter))
(define (ds-function-self-parameter? fp)
  (eqv? (ds-function-parameter-role fp) 'self))
(define (ds-function-input-parameter? fp)
  (eqv? (ds-function-parameter-role fp) 'in))
(define (ds-function-output-parameter? fp)
  (eqv? (ds-function-parameter-role fp) 'out))
(define (ds-function-parameter-name fp)
  (unless (ds-function-parameter? fp)
          (error "TypeError in ds-function-parameter-name"))
  (vector-ref fp 1))
(define (ds-function-parameter-type fp)
  (unless (ds-function-parameter? fp)
          (error "TypeError in ds-function-parameter-type"))
  (vector-ref fp 2))
(define (ds-function-parameter-init fp)
  (unless (ds-function-parameter? fp)
          (error "TypeError in ds-function-parameter-init"))
  (vector-ref fp 3))
(define (ds-function-parameter-role fp)
  (unless (ds-function-parameter? fp)
          (error "TypeError in ds-function-parameter-role"))
  (vector-ref fp 4))
(define (ds-function-parameter-meta fp)
  (unless (ds-function-parameter? fp)
          (error "TypeError in ds-function-parameter-meta"))
  (vector-ref fp 5))
(define (ds-function-parameter-meta-has? fp key)
  (let ((meta (ds-function-parameter-meta fp)))
    (hashtable-contains? meta key)))
(define (ds-function-parameter-meta-ref fp key)
  (let ((meta (ds-function-parameter-meta fp)))
    (hashtable-ref meta key)))
(define (ds-function-parameter-meta-set! fp key value)
  (let* ((meta (ds-function-parameter-meta fp))
         (hash (hashtable-set! meta key value)))
    (vector-set! fp 5 hash)
    fp))

(define (ds-function-parameter->gos-formal fp)
  (let ((name (ds-function-parameter-name fp))
        (type (ds-function-parameter-type fp)))
    (if name
        `#(,name ,type)
        type)))

; Drosera Function Class
;
; The methods are implemented as functions,
; where the first argument is a DsFunction object.
;
; Meta keys:
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
;
; define->procedure
; lambda->procedure
;
;   is-imported?
;   has-self?
;   has-ellipsis?
;
;
;  (function-parameter? ob)
;  (make-function-param name type init role meta)
;  role = one-of: 'in 'out 'self (default is in)
;  meta is an alist?
;core
;  (function-parameter-name fp)
;  (function-parameter-type fp)
;  (function-parameter-init fp)
;  (function-parameter-role fp)
;  (function-parameter-meta fp)
;query
;  (function-parameter-self? fp)
;  (function-parameter-input? fp)
;  (function-parameter-output? fp)
;
;
;  (function-signature? ob)
;core
;  (function-signature-meta fs)
;  (function-signature-parameters fs)
;query
;  (function-signature-self-parameter fs)
;  (function-signature-input-parameters fs)
;  (function-signature-output-parameters fs)
;  (function-signature-self-parameter fs)
;  (function-signature-self-parameter-name fs)
;  (function-signature-self-parameter-type fs)
;  (function-signature-input-parameters fs)
;  (function-signature-input-parameter-names fs)
;  (function-signature-input-parameter-types fs)
;  (function-signature-output-parameters fs)
;  (function-signature-output-parameter-names fs)
;  (function-signature-output-parameter-types fs)
;  (function-signature-output-parameter-type fs)
;  (function-signature-input-parameter-name proc k)
;  (function-signature-input-parameter-type proc k)
;meta
;  'go-multiple-return-values
;
;  (function? ob)
;core
;  (function-name fn)
;  (function-signature fn)
;  (function-meta fn)
;
;  function-self-name
; function-self-type
; function-input-names
; function-input-types
; function-input-type (query)
; function-output-names
; function-output-types
; function-output-type (query)
; function-body
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Methods
;;


;;
;; FunctionParameter constructors
;;

;; used all over, but mainly by (->ds-function)
;; accepts formals, 
;; no go:func magic here...
;; rest parameters may only be symbols
(define (->ds-function-input-parameters expr)
  (define (find-match obj . rest)
    (let ((role (if (pair? rest) (car rest) 'in)))
      (match obj
             ((#(name type) init)
              (make-ds-function-parameter name type init role))
             (#((name init) type)
              (make-ds-function-parameter name type init role))
             (#(name type)
              (make-ds-function-parameter name type (void) role))
             ((name init)
              (make-ds-function-parameter name 'go:any init role))
             (name
              (cond
               ((eqv? role 'in)
                (make-ds-function-parameter name 'go:any (void) role))
               ((eqv? role 'out)
                (make-ds-function-parameter #f name (void) role)))))))
  (define (->parameters obj . rest)
    (let* ((role (if (pair? rest) (car rest) 'in))
           (fp (find-match obj role))
           (name (ds-function-parameter-name fp))
           (init (ds-function-parameter-init fp))
           (type (ds-function-parameter-type fp)))
      (cond
       ((vector? name)
        (let* ((names (vector->list name))
               (type (last names))
               (names (most names)))
          (apply append (map (lambda (name)
            (->parameters `(#(,name ,type) ,init) role)) names))))
       ((list? name)
        (let* ((init (cdr name))
               (init (if (pair? init) (car init) (void)))
               (name (car name)))
          (list (make-ds-function-parameter name type init role))))
     (else (list fp)))))
  (define (ellipsis-parameter name)
    (make-ds-function-parameter name '(go:ellipsis go:any) (void) 'in '(rest)))
  (cond
   ((null? expr) '())
   ((pair? expr)
    (append (->parameters (car expr))
            (->ds-function-input-parameters (cdr expr))))
   (else (list (ellipsis-parameter expr)))))

(define (->ds-function-output-parameters type)
  (list (make-ds-function-parameter #f type (void) 'out)))

(define (->ds-function-self-parameters name type)
  (list (make-ds-function-parameter name type (void) 'self)))

;;(define (ds-function-parameter-ellipsis? fp)
;;  (ds-function-parameter-meta-has? fp 'go:ellipsis))



;;
;; dt:FunctionSignature
;;

;(define (->ds-function-signature ls)
;  (make-ds-function-signature (->ds-function-input-parameters ls)))

;(define (ds-function-signature-append fs1 fs2 . rest)
;  (let* ((fs1p (ds-function-signature-parameters fs1))
;         (fs1m (ds-function-signature-meta fs1))
;         (fs2p (ds-function-signature-parameters fs2))
;         (fs2m (ds-function-signature-meta fs2))
;         (fs3 (apply make-ds-function-signature (append fs1p fs2p)
;                     (hashtable->list (hashtable-append fs1m fs2m)))))
;    (if (null? rest)
;        fs3
;        (apply ds-function-signature-append fs3 rest))))





(define (->ds-function expr)
  (define (make-gos-type fs)
    (let* ((ins  (ds-function-signature->gos-input-parameters fs))
           (outs (ds-function-signature->gos-output-parameters fs)))
      `(go:func ,ins ,outs)))
  (define (make-name name fs)
    (make-ds-package-export name name (make-gos-type fs) (current-library-name)))
  (match expr

    (('define (#(sname stype) (? symbol? name) . sig) . body)
     (let ((fs (make-ds-function-signature (append
                  (->ds-function-self-parameters sname stype)
                  (->ds-function-input-parameters sig)
                  (->ds-function-output-parameters 'go:any)))))
       (make-ds-function (make-name name fs) fs body)))

    (('define ((? symbol? name) . sig) . body)
     (let ((fs (make-ds-function-signature (append
                  (->ds-function-input-parameters sig)
                  (->ds-function-output-parameters 'go:any)))))
       (make-ds-function (make-name name fs) fs body)))

    (('define (? symbol? name) ('lambda sig . body))
     (let ((fs (make-ds-function-signature (append
                  (->ds-function-input-parameters sig)
                  (->ds-function-output-parameters 'go:any)))))
       (make-ds-function (make-name name fs) fs body)))

    (('define (? symbol? name) ('case-lambda . bodies))
     (let ((fs (make-ds-function-signature (append
                  (->ds-function-input-parameters '(#(CaseRest (go:ellipsis go:any))))
                  (->ds-function-output-parameters 'go:any)))))
       (make-ds-function (make-name name fs) fs body)))

    (('lambda sig . body)
     (let ((fs (make-ds-function-signature (append
                  (->ds-function-input-parameters sig)
                  (->ds-function-output-parameters 'go:any)))))
       (make-ds-function #f fs body)))

    (('thunk . body)
     (let ((fs (make-ds-function-signature (append
                  (->ds-function-input-parameters '())
                  (->ds-function-output-parameters 'go:any)))))
       (make-ds-function #f fs body)))

    (('go:func #(sname stype) name insig outsig . body)
     (let ((fs (make-ds-function-signature (append
                  (->ds-function-self-parameters sname stype)
                  (->ds-function-input-parameters sig)
                  (->ds-function-output-parameters 'go:any)))))
       (make-ds-function (make-name name fs) fs body)))

    (('go:func (? symbol? name) insig outsig . body)
     (let ((fs (make-ds-function-signature (append
                  (->ds-function-input-parameters insig)
                  (->ds-function-output-parameters outsig)))))
       (make-ds-function (make-name name fs) fs body)))

    (('go:func insig outsig . body)
     (let ((fs (make-ds-function-signature (append
                  (->ds-function-input-parameters insig)
                  (->ds-function-output-parameters outsig)))))
       (make-ds-function #f fs body)))

    (else #f)))

;; dt:Function

(define (ds-function-imported? proc)
  (equal? (ds-function-meta-ref proc 'library-name)
          (current-library-name)))

(define (ds-function-library-name proc)
  ;(write (ds-function-meta-has? proc 'library-name))
  (if (ds-function-meta-has? proc 'library-name)
      (ds-function-meta-ref proc 'library-name)
      (current-library-name)))

(define (ds-function-mangled-library-name proc)
  (if (ds-function-meta-has? proc 'mangled-library-name)
      (ds-function-meta-ref proc 'mangled-library-name)
      (list->underscore (ds-function-library-name proc))))

(define (ds-function-mangled-name proc)
  (if (ds-function-meta-has? proc 'mangled-name)
      (ds-function-meta-ref proc 'mangled-name)
      (let ((mangled (symbol->mangle (ds-function-name proc))))
        (ds-function-meta-set! proc 'mangled-name mangled)
        mangled)))

(define (ds-function-parameters fn)
  (ds-function-signature-parameters
   (ds-function-signature fn)))

(define (ds-function-method? proc)
  (ds-function-meta-has? proc 'recv))

;; compiler logic

(define (ds-function->go-name proc)
  (if (ds-function-meta-has? proc 'go-name)
      (ds-function-meta-ref proc 'go-name)
      (string-append (ds-function-mangled-name proc))))

;(define (ds-function->go-body proc)
;  (let* ((stmts (map emit (ds-function-body proc)))
;         (block (string-join stmts "\n\t")))
;    (string-append "{\n\t" block "\n}\n")))
;
;(define (ds-function->go-func proc)
;  (if (ds-function-method? proc)
;      (ds-function->go-method-stmt proc)
;      (if (ds-function-name proc)
;          (ds-function->go-func-decl proc)
;          (ds-function->go-func-expr proc))))
;
;(define (ds-function->go-func-decl proc)
;  (let ((name (ds-function->go-name proc))
;        (sig  (ds-function->go-signature proc))
;        (body (ds-function->go-body proc)))
;    (string-append "go:func " name sig " " body)))
;
;(define (ds-function->go-func-expr proc)
;  (let ((sig  (ds-function->go-signature proc))
;        (body (ds-function->go-body proc)))
;    (string-append "go:func" sig " " body)))
;
;(define (ds-function->go-func-stmt proc)
;  (let ((name (ds-function->go-name proc))
;        (sig  (ds-function->go-signature proc))
;        (body (ds-function->go-body proc)))
;    (string-append "go:var " name " = go:func" sig " " body)))
;
;(define (ds-function->go-func-type proc)
;  (let ((sig  (ds-function->go-signature proc))
;        (body (ds-function->go-body proc)))
;    (string-append "go:func" sig)))
;
;(define (ds-function->go-method-stmt proc)
;  (let ((rec  (ds-function->go-receiver proc))
;        (name (ds-function->go-name proc))
;        (sig  (ds-function->go-signature proc))
;        (body (ds-function->go-body proc)))
;    (string-append "go:func " rec " " name sig " " body)))
;
;(define (ds-function->go-receiver proc)
;  (let* ((recv (ds-function-meta-ref proc 'recv))
;         (nm (symbol->string (vector-ref recv 0)))
;         (ty (emit (vector-ref recv 1))))
;    (string-append "(" nm " " ty ")")))
;
;(define (list->signature ls)
;  (ds-function-signature->gos 
;    (->ds-function-signature ls)))
;

(define (ds-function->go-library-name proc)
  (if (ds-function-meta-has? proc 'go-library-name)
      (ds-function-meta-ref proc 'go-library-name)
      (string-append "_" (ds-function-mangled-library-name proc))))

(define (ds-function->gos-import proc)
  (define (ds-function->gos-signature fn)
    (let ((fs (ds-function-signature fn)))
      (ds-function-signature->gos fs)))
  (let ((name  (symbol->string (ds-function-name proc)))
        (mname (string->symbol (ds-function-mangled-name proc)))
        (gname (ds-function->go-name proc))
        (lname (string->symbol (ds-function->go-library-name proc)))
        (sig   (ds-function->gos-signature proc)))
    `(go:= ,mname (go:as (go:index ,lname ,name) (go:func ,(most sig) ,(last sig))))))

;(define (ds-function->go-import proc)
;  (let ((sname  (ds-function-name proc))
;        (mname  (ds-function-mangled-name proc))
;        (name   (ds-function->go-name proc))
;        (lname  (ds-function->go-library-name proc))
;        (sig    (ds-function->go-signature proc)))
;    (if (not proc) ""
;    (string-append "var " mname " = " lname ".Ref("
;                   (emit-string (symbol->string sname)) ").(Named)\n"
;                   "var " name " = " mname ".Value().(func" sig ")\n"))))

(define (ds-function->gos-export proc)
  `(go:dot env (Add ,(ds-function-mangled-name proc))))

(define (ds-function->gos-func-head fn)
  (let* ((name (ds-function-name fn))
         (fs  (ds-function-signature fn))
         (self (ds-function-signature->gos-self-parameters fs))
         (ins  (ds-function-signature->gos-input-parameters fs))
         (outs (ds-function-signature->gos-output-parameters fs)))
    (if (null? self)
        (if (not name)
            `(go:func ,ins ,outs)
            `(go:func ,name ,ins ,outs))
        `(go:func ,@self ,name ,ins ,outs))))

;(define (ds-function->gos-define proc)
;  (let ((name (ds-function-name proc))
;        (sig  (ds-function-signature proc))
;        (body (ds-function-body proc)))
;    `(define ,(cons name sig) ,body)))

;(define (ds-function->gos-func-expr-head fn)
;  (let ((self (ds-function->gos-self-parameters fn))
;        (ins  (ds-function->gos-input-parameters fn))
;        (outs (ds-function->gos-output-parameters fn)))
;    `(go:func ,ins ,outs)))

;  (define (append-return body)
;    (let ((lb (last body))
;          (mb (most body)))
;      (if (and (pair? lb) (eqv? (car lb) 'go:return))
;          body (append mb (list (list 'go:return lb))))))
;(define (ds-function->gos-func-body fn)
;  (ds-function-body fn))

;(define (ds-function->gos-self-parameters fn)
;  (ds-function-signature->gos-self-parameters
;   (ds-function-signature fn)))
;
;(define (ds-function->gos-input-parameters fn)
;  (ds-function-signature->gos-input-parameters
;   (ds-function-signature fn)))
;
;(define (ds-function->gos-output-parameters fn)
;  (ds-function-signature->gos-output-parameters
;   (ds-function-signature fn)))
;
;(define (ds-function-self-parameters fn)
;  (ds-function-signature-self-parameters
;   (ds-function-signature fn)))
;
;(define (ds-function-input-parameters fn)
;  (ds-function-signature-input-parameters
;   (ds-function-signature fn)))
;
;(define (ds-function-output-parameters fn)
;  (ds-function-signature-output-parameters
;   (ds-function-signature fn)))

;(define (define->ds-function expr)
;  (match expr
;
;    (('define (name . sig) . body)
;     (make-ds-function name (->ds-function-signature sig)
;       (cons 'begin body) (cons 'library (current-library-name))))
;
;    (('define name ('lambda sig . body))
;     (make-ds-function name (->ds-function-signature sig)
;       (cons 'begin body) (cons 'library (current-library-name))))
;
;    (else #f)))
;
;  (let* ((r (cdr expr))
;         (sig (car r))
;         (body (cdr r))
;         (name (car sig))
;         (sig (cdr sig)))
;      (cons 'signature (append (list->signature sig) '(go:any)))
;
;  (when (not (eqv? (car expr) 'go:func))
;        (error "define-func->ds-function expected 'go:func, got:" expr))
;  (if (symbol? (car expr))
;
;      ;; function definition
;      (let* ((r (cdr expr))
;             (sig (car r))
;             (body (cdr r))
;             (name (car sig))
;             (sig (cdr sig))
;             (args (map vector-first (most sig))))
;        (make-ds-function name args
;             (cons 'signature sig)
;             (cons 'begin body)
;             (cons 'library (current-library-name))))
;
;      ;; method definition
;      (let* ((r (cdr expr))
;             (sig (car r))
;             (body (cdr r))
;             (recv (car sig))
;             (sig (cdr sig))
;             (name (car sig))
;             (sig (cdr sig))
;             (args (map vector-first (most sig))))
;        (make-ds-function name args
;             (cons 'go-name (symbol->string name))
;             (cons 'recv recv)
;             (cons 'signature sig)
;             (cons 'begin body)
;             (cons 'library (current-library-name))))))
;
;(define (lambda->ds-function expr)
;  (when (not (eqv? (car expr) 'lambda))
;        (error "lambda->ds-function expected 'lambda"))
;  (let* ((r (cdr expr))
;         (sig (car r))
;         (body (cdr r)))
;    (make-ds-function "" sig
;      (cons 'signature (append (list->signature sig) '(go:any)))
;      (cons 'begin (append-return body))
;      (cons 'library (current-library-name)))))

;(define (lambda-func->ds-function expr)
;  (when (not (eqv? (car expr) 'lambda-func))
;        (error "lambda-func->ds-function expected 'lambda-func"))
;  (let* ((r (cdr expr))
;         (sig (car r))
;         (body (cdr r))
;         (args (map vector-first (most sig))))
;    (make-ds-function "" args
;      (cons 'signature sig)
;      (cons 'begin body)
;      (cons 'library (current-library-name)))))

(define (ds-function->ds-library-env proc)
  (let ((name (ds-function-name proc))
        (sig  (ds-function-signature proc))
        (mods (ds-function-meta proc)))
    (apply list name sig (hashtable->list mods))))

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
;;   gos: (go:func + (#(x y go:any)) go:any ...) =>
;;   go: func __ZI(x, y interface{}) interface{} { ... }

;;  ;; UPDATED
;;  (set-optionals '())
;;  (cond
;;   ((null? ls) '())
;;   ((pair? ls)
;;    (let ((fs (car ls))
;;          (rs (cdr ls)))
;;      (cond
;;       ((null? fs) (error "list->signature" ls))
;;       ((vector? fs) (list fs (list->signature rs)))
;;       ;((pair? fs)
;;       ; (begin (set-optionals ls)
;;       ;        (list '(Rest (preellipsis Any)))))
;;       (else (cons `#(,fs go:any) (list->signature rs))))))
;;   (else (list `(,ls (preellipsis go:any))))))
;;
;;(define (list->signature-old ls)
;;  (define (argument-prelist2 ls)
;;    (cond
;;     ((null? ls) '())
;;     ((pair? ls)
;;      (let ((fst (car ls))
;;            (rst (cdr ls)))
;;        (cond
;;         ((list? fst)
;;          (begin (set-optionals ls)
;;                 (list `#(Rest go:any)))))))))
;;         ;(else
;;         ; 
;;         ;       (cons `(,(car ls) Any) (argument-prelist (cdr ls))))
;;         ;   (list `(,ls (preellipsis Any))))))))
;;  (define (argument-prelist ls)
;;    (if (null? ls)
;;        '()
;;        (if (pair? ls)
;;            (if (vector? (car ls))
;;                (begin
;;                  (set-optionals ls)
;;                  (list `(Rest (preellipsis Any))))
;;                (cons `(,(car ls) Any) (argument-prelist (cdr ls))))
;;            (list `(,ls (preellipsis Any))))))
;;  (set-optionals '())
;;  (let ((pre (argument-prelist ls)))
;;    ;;
;;    (if (and (pair? (last pre))
;;             (pair? (cdr (last pre)))
;;             (pair? (cadr (last pre))))
;;        (if (null? (most pre))
;;            (list (last pre))
;;            (list (append (map car (most pre)) '(Any)) (last pre) 'Boo))
;;        (if (null? pre) '()
;;            (list (append (map car pre) '(Any)))))))

;(define (ds-function-name-public? name)
;  (if (symbol? name)
;      (ds-function-name-public? (symbol->string name))
;      (if (and (string? name) (<= 1 (string-length name)))
;          (char-upper-case? (string-ref name 0))
;          #t)))
