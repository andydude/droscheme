(define (go-encoded? name)
  (if (>= (string-length name) 2)
      (let ((c0 (string-ref name 0))
            (c1 (string-ref name 1)))
        (and (eqv? c0 #\Z) (not (eqv? c1 #\Z))))
      #f))

(define (go-public? name)
  (ds-function-name-public? name))

(define (go-keyword? kw)
  (memv (string->symbol kw) go-keywords))

(define go-keywords '(
  append
  bool
  break
  byte
  cap
  case
  chan
  close
  complex
  complex128
  complex64
  const
  continue
  copy
  default
  defer
  delete
  else
  error
  fallthrough
  false
  float32
  float64
  for
  func
  go
  goto
  if
  imag
  import
  int
  int16
  int32
  int64
  int8
  interface
  iota
  len
  make
  map
  new
  nil
  package
  panic
  print
  println
  range
  real
  recover
  return
  rune
  select
  string
  struct
  switch
  true
  type
  uint
  uint16
  uint32
  uint64
  uint8
  uintptr
  var))

;; these 2 functions were added for racket compatibility

(define (list-opt ls a b)
  (if (> (length ls) a)
      (list-ref ls a)
      b))

(define (iota count . rest)
  (define (iota-3 count start step)
    (if (zero? count) '()
        (cons start (iota-3 (- count 1) (+ start step) step))))
  (let ((start (list-opt rest 0 0))
        (step  (list-opt rest 1 1)))
    (iota-3 count start step)))

;; these 3 functions were added for guile compatibility

(define (fold-right proc nil ls)
  (if (null? ls) nil
      (proc (car ls) (fold-right proc nil (cdr ls)))))

(define (most ls)
  (if (null? ls) '()
      (if (null? (cdr ls)) '()
          (cons (car ls) (most (cdr ls))))))

(define (last ls)
  (if (null? ls) ls
      (if (null? (cdr ls)) (car ls)
          (last (cdr ls)))))

;; Drosera encoding

(define (char-mangle c)
  (case c
    ((#\!) "ZA")
    ((#\#) "ZC")
    ((#\$) "ZD")
    ((#\%) "ZE")
    ((#\&) "ZF")
    ((#\*) "ZH")
    ((#\+) "ZI")
    ((#\,) "ZJ")
    ((#\-) "ZK")
    ;((#\.) "ZL")
    ((#\/) "ZM")
    ((#\:) "ZN")
    ((#\;) "ZO")
    ((#\<) "ZP")
    ((#\=) "ZQ")
    ((#\>) "ZR")
    ((#\?) "ZS")
    ((#\@) "ZT")
    ((#\^) "ZV")
    ((#\`) "ZW")
    ((#\|) "ZX")
    ((#\~) "ZY")
    ((#\Z) "ZZ")
    (else (string c))))

(define (char-unmangle c)
  (case c
    ((#\A) "!")
    ((#\C) "#")
    ((#\D) "$")
    ((#\E) "%")
    ((#\F) "&")
    ((#\H) "*")
    ((#\I) "+")
    ((#\J) ",")
    ((#\K) "-")
    ;((#\L) ".")
    ((#\M) "/")
    ((#\N) ":")
    ((#\O) ";")
    ((#\P) "<")
    ((#\Q) "=")
    ((#\R) ">")
    ((#\S) "?")
    ((#\T) "@")
    ((#\V) "^")
    ((#\W) "`")
    ((#\X) "|")
    ((#\Y) "~")
    ((#\Z) "Z")
    (else (string c))))

(define (string->mangle str)
  (string-join (map char-mangle (string->list str)) ""))

(define (symbol->mangle sym)
  (string->mangle (symbol->string sym)))

;(define (symbol-intern sym)
;  (string->symbol (string-append "_" (symbol->string sym))))

;; ======================================================================
;; object-oriented definitions
;; ======================================================================

(define (make-ds-library name exports imports defines . dictionary)
  `#(droscheme-library ,name ,exports ,imports ,defines
    ,(list->eqv-hashtable dictionary)))

(define (ds-library-name proc)
  (vector-ref proc 1))

(define (ds-library-exports proc)
  (vector-ref proc 2))

(define (ds-library-imports proc)
  (vector-ref proc 3))

(define (ds-library-defines proc)
  (vector-ref proc 4))

(define (ds-library-attributes proc)
  (vector-ref proc 5))

(define (list->ds-library spec)
  (let* ((path (list->directory spec))
         (name (list->underscore spec))
         (filename (string-append (droscheme-path) "/src/" path "/" name ".sld"))
         (expr (call-with-input-file filename read))
         (libname (cadr expr))
         (exports (assoc 'export (cdr expr)))
         (imports (assoc 'import (cdr expr)))
         (defines (assoc 'begin (cdr expr))))
    (make-ds-library libname 
      (if exports (cdr exports) '())
      (if imports (cdr imports) '())
      (if defines (cdr defines) '())
      `(library ,spec))))

(define (ds-library->go-import libr)
  (define (decl->func decl)
    (let ((kind (car decl)))
      (cond
       ((eqv? kind 'define)
        (define->ds-function decl))
       ((eqv? kind 'define-func)
        (define-func->ds-function decl))
       (else #f))))
  (let* ((defines (ds-library-defines libr))
         (funcs (map decl->func defines))
         (decls (map ds-function->go-import funcs)))
    (string-join decls "\n")))

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
  (hashtable-set! (ds-function-attributes proc) key value))

(define (ds-function-imported? proc)
  (equal? (ds-function-attributes-ref proc 'library-name)
          (current-library-name)))

(define (ds-function-library-name proc)
  (ds-function-attributes-ref proc 'library))

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
  (let ((name  (emit-string (symbol->string (ds-function-name proc))))
        (mname (ds-function-mangled-name proc))
        (gname (ds-function->go-name proc))
        (lname (ds-function->go-library-name proc))
        (sig   (ds-function->gos-signature proc)))
    `(define-var (,mname ,gname) (as (dot ,lname (Ref ,name)) Named)
        (as (dot ,mname (Value)) (func sig)))))

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
        (body (ds-function-body proc)))
    `(define-func ,(cons name sig) ,body)))

(define (define->ds-function expr)
  (when (not (eqv? (car expr) 'define))
        (error "define->ds-function expected 'define, got:" expr))
  (let* ((r (cdr expr))
         (sig (car r))
         (body (cdr r))
         (name (car sig))
         (sig (cdr sig)))
    (make-ds-function name sig
      (cons 'signature (append (list->signature sig) '(Any)))
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
      (cons 'signature (append (list->signature sig) '(Any)))
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
       (else (cons `#(,fs Any) (list->signature rs))))))
   (else (list `(,ls (preellipsis Any))))))

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

;(define (emit-top-level-begin . o))
;(define (emit-top-level-package . o))
;(define (emit-top-level-library . o))
;(define (emit-top-level-define-library . o))
;(define (emit-top-level-ns . o))

(define (map-emit emit ls)
  (string-join (map emit (map car bindings)) ", "))

(define (emit-let-bindings-cadrs bindings)
  (string-join (map emit (map cadr bindings)) ", "))

(define (list->go-block ls)
  (string-join (map emit ls) "\n"))

(define (list->go-list ls)
  (string-join (map emit ls) ", "))

(define (vector->go-list vc)
  (string-join (map emit (vector->list vc)) ", "))

(define (emit-++ x)
  (string-append (emit x) "++"))

(define (emit--- x)
  (string-append (emit x) "--"))

(define (emit-+ . rest)
  (string-join (map emit rest) " + "))

(define (emit-* . rest)
  (string-append "(" (string-join (map emit rest) " * ") ")"))

(define (emit-!= a b)
  (string-append "(" (emit a) " != " (emit b) ")"))
(define (emit-== a b)
  (string-append "(" (emit a) " == " (emit b) ")"))

(define (emit-<2 a b)
  (string-append "(" (emit a) " < " (emit b) ")"))
(define (emit-< . rest)
  (emit-list-and (map emit-<2 (most rest) (cdr rest))))
(define (emit->2 a b)
  (string-append "(" (emit a) " > " (emit b) ")"))
(define (emit-> . rest)
  (emit-list-and (map emit->2 (most rest) (cdr rest))))
(define (emit-<=2 a b)
  (string-append "(" (emit a) " <= " (emit b) ")"))
(define (emit-<= . rest)
  (emit-list-and (map emit-<=2 (most rest) (cdr rest))))
(define (emit->=2 a b)
  (string-append "(" (emit a) " >= " (emit b) ")"))
(define (emit->= . rest)
  (emit-list-and (map emit->=2 (most rest) (cdr rest))))

(define (emit-<- a)
  (string-append "<-" (emit a)))
(define (emit-<-! a b)
  (string-append (emit a) "<-" (emit b)))
(define (emit-array n t)
  (string-append "[" (emit-number n) "]" (emit t)))
(define (emit-array... t)
  (string-append "[...]" (emit t)))

(define (emit-adr expr)
  (string-append "&" (emit expr)))

(define (emit-to-bool cond)
  (define (scheme-function? expr)
    (let* ((s (emit expr))
           (a (string-ref (emit expr) 0)))
      (if (eqv? a #\_) #t
          (if (eqv? a #\!)
              (if (eqv? (string-ref (emit expr) 2) #\_) #t #f)
              #f))))
  (if (scheme-function? cond)
      (string-append (emit cond) ".(bool) ")
      (emit cond)))

(define (emit-body proc rest)
  (string-join (map proc rest) "\n\t"))

;(define (emit-toa ty ob)
;  (string-append (emit ty) "(" (emit ob) ")"))
;
;(define (emit-isa ob ty)
;  (string-append (emit ob) ".(" (emit ty) ")"))

(define (emit-if1 condition . rest)
  (string-append "if "
     (emit-to-bool condition) " {\n"
     (string-join (map emit rest) "\n\t")
     "\n}\n"))

(define (emit-if2 opt condition . rest)
  (string-append "if "
     (string-append (emit opt) "; ")
     (emit-to-bool condition) " {\n"
     (string-join (map emit rest) "\n\t")
     "\n}\n"))

(define (emit-if condition then . rest)
  (if (pair? rest)
      (string-append "func()Any{\nif "  (emit-to-bool condition)
                     (emit-braces-return emit (list then))
                     "\nreturn " (emit (car rest))
                     "\n}()\n")
      (string-append "func()Any{\nif "  (emit-to-bool condition)
                     (emit-braces-return emit (list then))
                     "\nreturn _void()\n}()\n")))

(define (emit-dot . rest)
  (string-join (map emit rest) "."))

(define (emit-null dummy) "_null()")

(define (emit-len x)
  (string-append "len(" (emit x) ")"))

(define (emit-let-bindings-cars bindings)
  (list->go-list (map car bindings)))

(define (emit-let-bindings-cadrs bindings)
  (list->go-list (map cadr bindings)))

(define (emit-let-bindings-seq bindings)
  (define (var-gos x)
    `(define-var (= ,(car x) ,(cadr x))))
  (list->go-block (map var-gos bindings)))

(define (emit-let-seq ty bindings . rest)
  (string-append "func()" (emit ty)
                 (emit-braces-stuff-return
                  (emit-let-bindings-seq bindings) emit rest)
                 "()"))

(define (emit-let-bind ty bindings . rest)
  (string-append "func(" (emit-let-bindings-cars bindings) " Any)" (emit ty)
                 (emit-braces-return emit rest)
                 "(" (emit-let-bindings-cadrs bindings) ")"))

(define (emit-let-loop ty name bindings . rest)
  (let ((recur (string-append "_" (emit name))))
    (string-append "func()Any{\n" "var "
                   recur " func(" (emit-let-bindings-cars bindings) " Any)" (emit ty) "\n"
                   recur " = func(" (emit-let-bindings-cars bindings) " Any)" (emit ty)
                   (emit-braces-return emit rest)
                   "\nreturn " recur "(" (emit-let-bindings-cadrs bindings) ")"
                   "\n}()\n")))

(define (emit-let* frst . rest)
  (if (list? frst)
      (apply emit-let-seq 'Any frst rest)
      (error "let* does not support looping")))

(define (emit-let frst . rest)
  (if (list? frst)
      (apply emit-let-bind 'Any frst rest)
      (apply emit-let-loop 'Any frst rest)))

(define (emit-type-name a)
  (emit-symbol a))
;  (if (symbol? a)
;      (symbol->string a)
;      (emit a)))
      
(define (emit-as a b . rest)
  (if (null? rest)
      (string-append (emit a) ".(" (emit-type-name b) ")")
      (apply emit-dot (list 'as a b) rest)))

(define (emit-else . rest)
  (string-append "\n} else {\n\t"
     (string-join (map emit rest) "\n\t")))

(define (emit-map-type a b)
  (string-append "map[" (emit a) "]" (emit b)))

(define (emit-function-name fn)
  (let ((out (emit fn)))
    (string-append
     (if (ds-function-name-public? fn) "" "")
     (if (and (>= (string-length out) 2)
              (equal? (substring out 0 2) "__"))
         (substring out 2)
         out))))
        
(define (emit-apply... proc . rest)
  (string-append
    (emit-function-name proc)
    "(" (list->go-list rest) "...)"))

(define (emit-call proc . rest)
  (string-append
    (emit-function-name proc)
    "(" (list->go-list rest) ")"))

(define (emit-gocall proc . rest)
  (string-append
    (symbol->string proc)
    "(" (list->go-list rest)")"))

(define (emit-not a)
  (string-append "! " (emit a)))
(define (emit-list-and ls . rest)
  (string-append "(" (string-join ls " && ") ")"))
(define (emit-list-or ls . rest)
  (string-append "(" (string-join ls " || ") ")"))
(define (emit-and . rest)
  (string-append "(" (string-join (map emit rest) " && ") ")"))
(define (emit-or . rest)
  (string-append "(" (string-join (map emit rest) " || ") ")"))

(define (emit-bool b) (if b "true" "false"))
(define (emit-char c) (string-append "'" (string-escape (list->string (list c))) "'"))

(define (emit-chan t) (string-append "chan " (emit-type-name t)))
(define (emit-chan<- t) (string-append "<-chan " (emit-type-name t)))
(define (emit-chan<-! t) (string-append "chan<- " (emit-type-name t)))

(define (emit-comment text) (string-append "// " text "\n"))

(define (emit-preellipsis name)
  (string-append "..." (emit name)))

(define (emit-postellipsis name)
  (string-append (emit name) "..."))

;; TODO: rethink combination
(define (emit-environment . libs)
  (define (symbol->symbol spec)
    (string->symbol (string-join (map symbol->string spec) "_")))
  ;(error "environment: " (cadar libs))
  (if (= (length libs) 1)
      (let ((libspec (cadar libs)))
        (emit `(dot ,(symbol->symbol libspec) (Export) (Extend))))
      (list->go-block (map emit-environment libs))))

(define (emit-lambda . rest)
  (ds-function->go-func-expr
   (lambda->ds-function
    (cons 'lambda rest))))
  ;(apply emit-define-func (cons "" fsig) rest))

(define (emit-lambda-func . rest)
  (ds-function->go-func-expr
   (lambda-func->ds-function
    (cons 'lambda-func rest))))

(define (emit-lambda... . rest) "")
(define (emit-define... . rest) "")
(define (emit-define-func-blank . rest)
  "")

(define (emit-define-func-env fsig . rest)
  (let ((fn (if (symbol? (car fsig))
                (car fsig)
                (cadr fsig))))
    (string-append
     (if (ds-function-name-public? fn)
         ""
         (string-append
          "var " (emit-symbol fn) " = _"
          (*package-name*) ".Ref(" (emit-string (symbol->string fn)) ").(Named)\n"
          "var " (emit-function-name fn) " = "
          (emit-symbol fn) ".Value().(func"
          ;`(define-func ,fsig ,@rest)
          (emit-lambda-signature (cdr fsig)) ")\n")))))

(define (emit-define-func-compile fsig . rest)
  (let ((func (define-func->ds-function
                `(define-func ,fsig ,@rest))))
    (ds-function->go-func func)))

(define (emit-define-func-export fsig . rest)
  "")

(define (emit-define-func-import fsig . rest)
  (let ((fn (if (symbol? (car fsig))
                (car fsig)
                (cadr fsig))))
    (string-append
     (if (ds-function-name-public? fn)
         ""
         (string-append
          "var " (emit-symbol fn) " = _"
          (*package-name*) ".Ref(" (emit-string (symbol->string fn)) ")\n"
          "var _" (emit-symbol fn) " = "
          (emit-symbol fn) ".(Named).Value().(func" 
          (emit-lambda-signature (cdr fsig)) ")\n")))))

;(define (argument-preprelist arg)
;  (if (vector? arg)

(define (emit-define-optional k v)
  (let* ((idx (number->string k))
         (rst (string-append "Rest[" idx "]"))
         (key (vector-ref v 0))
         (val (vector-ref v 1)))
  `(:= ,key (if (> (call len Rest) ,k)
                (inline ,rst)
                ,val))))

;(inline ,(string-append "Rest[" idx "]"))
;(emit (vector-ref v 0)) "x = " (emit v) (number->string k) ";")))

(define (emit-define-return rt fsig . rest)
  (if (symbol? fsig)
      (apply emit-define-func fsig rest)
      (begin
        (append-defines (car fsig))
        (if (pair? fsig)
            (apply emit-define-func
                   (append (list (car fsig)) (list->signature (cdr fsig)) (list rt))
                   (append (map emit-define-optional (iota (length (*optionals*)))
                                (*optionals*)) (most rest)
                                (list (cons 'return (list (last rest))))))
            "//WHAT?"))))

(define (emit-define fsig . rest)
  (apply emit-define-return 'Any fsig rest))

(define (emit-define-bool fsig . rest)
  (apply emit-define-return 'bool fsig rest))


(define (emit-expr-cond-clause clause)
  (string-append
   (if (eqv? (car clause) 'else)
       "default:\n" 
       (string-append
        "case " (emit (car clause)) ":\n"))
   (list->go-block (cdr clause))))

(define (emit-expr-case-clause clause)
  (string-append
   (if (eqv? (car clause) 'else)
       "default:\n" 
       (string-append
        "case " (list->go-list (car clause)) ":\n"))
   (list->go-block (cdr clause))))

(define (emit-switch-cond . rest)
  (string-append
    "switch "
    (emit-braces emit-expr-cond-clause rest)))
(define (emit-switch-cond* a . rest)
  (string-append
    "switch " (emit a) "; "
    (emit-braces emit-expr-cond-clause rest)))
(define (emit-switch-case b . rest)
  (string-append
    "switch " (emit b)
    (emit-braces emit-expr-case-clause rest)))
(define (emit-switch-case* a b . rest)
  (string-append
    "switch " (emit a) "; " (emit b)
    (emit-braces emit-expr-case-clause rest)))

(define (emit-switch-type b . rest)
  (string-append
    "switch " (emit b)
    (emit-braces emit-expr-case-clause rest)))
(define (emit-switch-type* a b . rest)
  (string-append
    "switch " (emit a) "; " (emit b)
    (emit-braces emit-expr-case-clause rest)))

;(define (emit-select-comm-clause clause)
;  (string-append
;   (if (eqv? (car clause) 'else)
;       "default:\n" 
;       (string-append
;        "case " (emit (car clause)) ":\n"))
;   (list->go-block (cdr clause))))

(define (emit-select-comm . rest)
  (string-append
    "select "
    (emit-braces emit-expr-cond-clause rest)))

(define (emit-for1 c . rest)
  (string-append
    "for "
    (emit-to-bool c)
    (emit-braces emit rest)))

(define (emit-for2 c . rest)
  (string-append
    "for "
    (apply (lambda (sym lhs . rhs)
       (emit-assign #f ":= range " lhs rhs)) c)
    ;(emit `(,(car c) ,(cadr c) (inline " range ") ,@(cddr c)))
    (emit-braces emit rest)))

(define (emit-for3 c b a . rest)
  (string-append
    "for "
    (emit c)
    "; "
    (emit-to-bool b)
    "; "
    (emit a)
    (emit-braces emit rest)))

(define (emit-func . rest)
  (emit-signature rest))

(define (emit-inline . rest)
  (apply string-append rest))

(define (list->directory libspec)
  (if (string? libspec)
      libspec
      (if (list? libspec)
          (string-join (map emit libspec) "/")
          (symbol->string libspec))))

(define (underscore->list libspec)
  (string-split libspec "_"))

(define (list->underscore libspec)
  (if (string? libspec)
      libspec
      (if (list? libspec)
          (string-join (map emit libspec) "_")
          (symbol->string libspec))))

(define (emit-import-spec spec)
  (if (string? spec)
      (emit-string spec)
      (if (list? spec)
          (cond
            ((eqv? (car spec) 'as)
             (string-append (emit-symbol (cadr spec)) " "
                            (emit-string (list->directory (caddr spec)))))
            ((eqv? (car spec) 'dot)
             (string-append ". " (emit-string (list->directory (cadr spec)))))
            (else
             (emit-string (list->directory spec))))
          (error "import expected string or list"))))

(define (emit-import-name spec)
  (if (string? spec)
      (emit-string spec)
      (if (list? spec)
          (cond
           ((eqv? (car spec) 'as) "//as\n")
           ((eqv? (car spec) 'dot) "//dot\n")
           ((eqv? (car spec) 'only) "//only\n")
           ((eqv? (car spec) 'prefix) "//prefix\n")
           ((eqv? (car spec) 'remove-prefix) "//remove-prefix\n")
           ((eqv? (car spec) 'rename) "//rename\n")
           (else
            (parameterize ((*package-path* (list->directory spec))
                           (*package-name* (list->underscore spec)))
              (string-append
               (emit-import-variable spec)
               (ds-library->go-import (list->ds-library spec))))))
          (error "import expected string or list"))))

(define (emit-import-variable spec)
  (let ((name (list->underscore spec)))
    (string-append
     "var _" name " = " name ".Export()\n")))

(define (emit-import-names specs)
  (map emit-import-name specs))

(define (emit-import-blank . rest) "")

(define (emit-import-import . rest)
  (string-append
   (emit-parens "import" emit-import-spec rest)
   (string-join (emit-import-names rest) "\n")))

(define (emit-import-compile . rest)
  (emit-parens "import" emit-import-spec rest))

;  (if (pair? rest)
;      (let ((cmd (car rest))
;            (cmds (cdr rest)))
;        (if (eqv? (car cmd) 'import)
;            (cons (apply emit-import (cdr cmd))
;                  (apply emit-package-begin cmds))
;            (cons ""
;                  (apply emit-package-begin cmds))))
;      (error "emit-package-begin expected list?")))

(define (emit-import-env . rest) "")
(define (emit-import-export . rest) "///import\n")
;;  (emit-parens "import" emit-import-spec rest))
;       (if (pair? rest)
;           (let ((cmd (car rest))
;                 (cmds (cdr rest)))
;-            (if (eqv? (car cmd) 'begin)
;-                (map emit (cdr cmd))
;-                (cons (emit cmd)
;+            (if (eqv? (car cmd) 'import)
;+                (cons (apply emit-import (cdr cmd))
;+                      (apply emit-package-begin cmds))
;+                (cons ""
;                       (apply emit-package-begin cmds))))
;           (error "emit-package-begin expected list?"))))

(define (emit-export . rest)
  (*has-export* #t)
  (*defines* (reverse rest))
  "") ; must emit a string

(define (emit-interface . rest)
  (string-append "interface" (apply emit-interface-body rest)))

(define (emit-interface-body . rest)
  (if (null? rest) "{}"
      (emit-braces emit-interface-method rest)))

(define (emit-interface-method method)
  (if (symbol? method)
      (emit-symbol method)
      (emit-signature method)))

(define (emit-braces proc rest)
  (if (null? rest) "\n"
      (string-append " {\n\t" (string-join (map proc rest) "\n\t") "\n}\n")))

(define (emit-braces-stuff-return stuff proc rest)
  (if (null? rest) "\n"
      (string-append " {\n\t" stuff
       (string-join (map proc (most rest)) "\n\t") 
       "\n\treturn " (proc (last rest)) "\n}")))

(define (emit-braces-return proc rest)
  (if (null? rest) "\n"
      (string-append " {\n\t" 
       (string-join (map proc (most rest)) "\n\t") 
       "\n\treturn " (proc (last rest)) "\n}")))

(define (emit-parens kw proc rest)
  (if (= (length rest) 1)
      (string-append kw " " (proc (car rest)) "\n")
      (string-append kw " (\n\t" (string-join (map proc rest) "\n\t") "\n)\n")))

(define (emit-method-signature fsig)
  (cond
   ((pair? (car fsig))
    ;; method
    (let ((rcv (car fsig))
          (sig (cdr fsig)))
      (string-append
       (emit-signature-args (list rcv))
       " "
       (emit-signature sig))))
   ((symbol? (car fsig))
    ;; function
    (emit-signature fsig))))

(define (emit-number n) (number->string n))

(define (emit-define-library-begin . rest)
  (if (null? rest)
      '()
      (if (pair? rest)
          (let ((cmd (car rest))
                (cmds (cdr rest)))
            (if (eqv? (car cmd) 'begin)
                (map emit (cdr cmd))
                (cons (emit cmd)
                      (apply emit-define-library-begin cmds))))
          (error "emit-define-library-begin expected list?"))))

(define (emit-package-blank . rest) "")

(define (emit-package-env name . rest)
  (apply string-append
         "var _"
         (*package-name*) " = "
         (*package-name*) ".Export()\n"
         (append (apply emit-define-library-begin rest)
                 (list (emit-defines)))))

(define (emit-package-import name . rest)
  (apply string-append
         "package " (list->underscore name) "\n\n"
         "import . \"ds/any\"\n\n"
         (append (apply emit-define-library-begin rest)
                 (list (emit-defines)))))

(define (emit-package-export name . rest)
  (apply string-append
         "package " (list->underscore name) "\n\n"
         "import . \"ds/any\"\n\n"
         (append (apply emit-define-library-begin rest)
                 (list (emit-defines)))))

(define (emit-package-compile name . rest)
  (apply string-append
         "package " (list->underscore name) "\n\n"
         (apply emit-define-library-begin rest)))

(define (emit-package-package name . rest)
  (apply string-append
         "package " (list->underscore name) "\n\n"
         (apply emit-define-library-begin rest)))

(define (emit-ptr t)
  (string-append "*" (emit t)))

(define (emit-index t k . r)
  (define (emit-offset k)
    (if (not k)
        ""
        (emit k)))
  (if (null? r)
      (string-append (emit t) "[" (emit-offset k) "]")
      (string-append (emit t) "[" (emit-offset k) ":" (emit-offset (car r)) "]")))

(define (emit-lambda-signature args)
    (if (null? args) "()"
        (let ((ag (most args))
              (rs (last args)))
          (string-append
           (emit-signature-args ag)
           (if (eqv? rs '&void)
               ""
               (if (pair? rs)
                   (let ((r (car rs)))
                     (if (eqv? r 'void)
                         ""
                         (if (eqv? r 'values)
                             (emit-signature-args (cdr rs))
                             (emit-signature-args (list rs)))))
                   (emit rs)))))))

(define (emit-signature sig)
  (let ((fn (car sig))
        (ar (cdr sig)))
    (string-append
     (emit-function-name fn)
     (emit-lambda-signature ar))))

(define (emit-signature-arg arg)
  ;; UPDATED
  (if (pair? arg)
      (emit arg)
      (if (vector? arg)
          (let ((vs (vector-most arg)) (ts (vector-last arg)))
            (string-append (vector->go-list vs) " " (emit ts)))
          (if (symbol? arg)
              (emit-symbol arg)
              (error "WHAT?!?!?" arg)))))

(define (emit-signature-args sig)
  (string-append "(" (string-join (map emit-signature-arg sig) ", ") ")"))


(define (emit-slice t) (string-append "[" "]" (emit-type-name t)))

;; TODO: character escapes
(define (emit-string s)
  (string-append "\"" (string-escape s) "\""))

(define (emit-struct . rest)
  (string-append "struct" (apply emit-struct-body rest)))

(define (emit-struct-body . rest)
  (if (null? rest) "{}"
      (emit-braces emit-struct-field rest)))

(define (emit-struct-field field)
  (if (symbol? field)
      (emit-symbol field)
      (emit-signature-arg field)))

(define (emit-symbol id)
  (define symbol-table '(
          ; types
          ("&byte" . "byte")
          ("&complex64" . "complex64")
          ("&complex128" . "complex128")
          ("&float32" . "float32")
          ("&float64" . "float64")
          ("&int" . "int")
          ("&int8" . "int8")
          ("&int16" . "int16")
          ("&int32" . "int32")
          ("&int64" . "int64")
          ("&rune" . "rune")
          ("&string" . "string")
          ("&uint" . "uint")
          ("&uint8" . "uint8")
          ("&uint16" . "uint16")
          ("&uint32" . "uint32")
          ("&uint64" . "uint64")
          ("&uintptr" . "uintptr")
          ("&void" . "")
          ; objects
          ("%nil" . "nil")))
  (if (symbol? id)
      (let ((s (symbol->string id)))
        (if (or (eqv? (string-ref s 0) #\%)
                (eqv? (string-ref s 0) #\&))
            (cdr (assoc s symbol-table))
            (let ((out (symbol->mangle id)))
              (if (or (go-keyword? out)
                      (go-encoded? out))
                  (string-append "__" out)
                  out))))
      (error id)))

(define (emit-type . rest)
  (emit-parens "type" emit-type-spec rest))

(define (emit-type-spec spec)
  (let ((nm (car spec))
        (df (cadr spec)))
    (string-append (emit-symbol nm) " " (emit df) "\n")))

(define (emit-= lhs . rhs)
  (emit-assign #f "=" lhs rhs))

(define (emit-=: lhs . rhs)
  (emit-assign #t "=" lhs rhs))

(define (emit-:= lhs . rhs)
  (emit-assign #f ":=" lhs rhs))

(define (emit-:=: lhs . rhs)
  (emit-assign #t ":=" lhs rhs))

(define (emit-:: lhs rhs)
  (string-append (emit lhs) " " (emit rhs)))
;;(emit-signature-arg lhs))

(define (emit-assign typed op lhs rhs)
  (if (pair? lhs)
      (string-append
       ((if typed
            emit-signature-arg
            list->go-list) lhs) " " op " "
       (list->go-list rhs))
      (string-append
       (emit lhs) " " op " " (emit (car rhs)))))

(define (emit-const . rest)
  (emit-parens "const" emit-const-spec rest))

(define (emit-const-spec ls)
  (if (eqv? '= (car ls))
      (apply emit-= (cdr ls))
      (emit-signature-arg ls)))

(define (emit-return . rest)
  (if (null? rest) "return"
      (string-append "return " (list->go-list rest))))

(define (emit-var . rest)
  (emit-parens "var" emit-var-spec rest))

(define (emit-var-spec ls)
  (if (not (pair? ls))
      (emit-symbol ls)
      (cond
       ((member (car ls) '(= =: ::)) (emit ls))
       (else (error "expected something else" ls)))))
;  (if (eqv? '= (car ls))
;      (apply emit-= (cdr ls))
;      (emit-signature-arg ls)))

(define (emit-keyword kw)
  (string-append (keyword->string kw) ":"))

(define (emit-literal-part part)
  (if (keyword? part)
      (emit part)
      (string-append (emit part) ",")))

(define (emit-literal vec)
  (let* ((l (vector->list vec))
         (a (car l))
         (r (cdr l)))
    (string-append
      (emit a)
      "{"
      (string-join (map emit-literal-part r) " ")
      "}")))

(define (emit-continue . rest)
  "continue")
(define (emit-break . rest)
  "break")
(define (emit-goto label)
  (string-append "goto " (emit label)))
(define (emit-defer expr)
  (string-append "defer " (emit expr)))
(define (emit-fallthrough . rest)
  "fallthrough")

(define (basename st)
  (if (not (string? st))
      (symbol->string st)
      (if (not (string-index-right st #\/))
          (substring st 0 (- (string-length st) 4))
          (let ((start (string-index-right st #\/))
                (end (string-length st)))
            (substring st (+ start 1) (- end 4))))))

(define (emit-defines-blank) "")

(define (emit-defines-export)
  (define (emit-defines-registration def)
    `(dot env (Add ,def)))
  (let ((name (basename (*input-filename*))))
    (apply emit-define-func-compile
           '(Export (env (ptr Env)))
           (append
            (list '(= env (NewEnv)))
            (map emit-defines-registration (reverse (*defines*)))
            (list '(return))))))

(define (emit-defines-import) "")

(define (emit-defines-env)
  (define (emit-defines-registration def)
    `(dot env (Add ,def)))
  (let ((name (basename (*input-filename*))))
    (apply emit-define-func
           '(Export (env (ptr Env)))
           (append
            (list '(= env (NewEnv)))
            (map emit-defines-registration (reverse (*defines*)))
            (list '(return))))))

(define (append-defines name)
  (if (*has-export*)
      #f
      (*defines* (cons name (*defines*)))))

(define (set-optionals name)
  (*optionals* name))

(define (droscheme-path)
  (let ((path (get-environment-variable "DROSCHEME_PATH")))
    (if path
        path
        (string-append (get-environment-variable "HOME") "/.droscheme"))))

(define (current-library-name)
  (*package-name*))

(define (eval-symbol y)
  (if (symbol? y)
      (cdr (assoc y (syntax-table)))
      (error "expected symbol")))

(define (emit-void . r) "")

(define (emit expr)
  (if (pair? expr)
      (if (assoc (car expr) (syntax-table))
          (let* ((y (car expr))
                 (f (eval-symbol y)))
            (apply f (cdr expr)))
          (apply emit-call expr))
      (cond
       ((char? expr) (emit-char expr))
       ((boolean? expr) (emit-bool expr))
       ((number? expr) (emit-number expr))
       ((string? expr) (emit-string expr))
       ((symbol? expr) (emit-symbol expr))
       ((keyword? expr) (emit-keyword expr))
       ((vector? expr) (emit-literal expr))
       ((null? expr) (emit-null expr))
       (else (error "What?!?")))))

(define (syntax-table)
  (list
    (cons '<-             emit-<-         )
    (cons '<-!            emit-<-!        )
    (cons '*              emit-*          )
    (cons '+              emit-+          )
    (cons '++             emit-++         )
    (cons '--             emit---         )
    (cons '::             emit-::         )
    (cons ':=             emit-:=         )
    (cons ':=:            emit-:=:        )
    (cons '<              emit-<          )
    (cons '<=             emit-<=         )
    (cons '=              emit-=          )
    (cons '=:             emit-=:         )
    (cons '==             emit-==         )
    (cons '!=             emit-!=         )
    (cons '>              emit->          )
    (cons '>=             emit->=         )
    (cons 'adr            emit-adr        ) ; &a
    (cons 'and            emit-and        ) ; a && b
    (cons 'apply...       emit-apply...   ) ; f(x...)
    (cons 'array          emit-array      ) ; [a]b
    (cons 'slice          emit-slice      )
    (cons 'array...       emit-array...   ) ; [...]b
    (cons 'as             emit-as         ) ; a.(b).c().(d).e()
    (cons 'break          emit-break      )
    (cons 'call           emit-gocall     )
    (cons 'map-type       emit-map-type     )
;    (cons 'call           emit-call       )
;    (cons 'case           emit-case       )
    (cons 'chan           emit-chan       )
    (cons 'chan<-         emit-chan<-     )
    (cons 'chan<-!        emit-chan<-!    )
    (cons 'comment        emit-comment    )
    (cons 'continue       emit-continue   )
    (cons 'defer          emit-defer      )
    (cons 'define         emit-define     )
    (cons 'define-const   emit-const      )
    (cons 'define-func    emit-define-func)
    (cons 'define-func... emit-define...  )
    (cons 'define-type    emit-type       )
    (cons 'define-library emit-package) ; package
    (cons 'define-var     emit-var        )
    (cons 'environment    emit-environment)
    (cons 'dot            emit-dot        ) ; go a.b().c().d().e()
;    (cons 'else           emit-else       )
    (cons 'export         emit-export     )
    (cons 'fallthrough    emit-fallthrough)
    (cons 'for            emit-for3       )
;    (cons 'func           emit-func       )
;    (cons 'go             emit-go         )
    (cons 'goto           emit-goto       )
    (cons 'if             emit-if         )
    (cons 'when           emit-if1        ) ; if c {}
    (cons 'when*          emit-if2        ) ; if stmt; c {}
    (cons 'import         emit-import     )
    (cons 'index          emit-index      ) ; a[b], a[b:c]
    (cons 'inline         emit-inline     )
    (cons 'interface      emit-interface  )
    (cons 'lambda         emit-lambda     )
    (cons 'lambda-func    emit-lambda-func)
    (cons 'lambda-func... emit-lambda...  )
    (cons 'len            emit-len        )
    (cons 'let            emit-let        )
    (cons 'let*           emit-let*       )
;    (cons 'map            emit-map        )
    (cons 'not            emit-not        )
    (cons 'or             emit-or         ) ; a || b
    (cons 'package        emit-package    ) ; package
;    (cons 'postellipsis   emit-postellipsis)
;    (cons 'preellipsis    emit-preellipsis)
    (cons 'ptr            emit-ptr        )
    (cons 'range          emit-for2      )
    (cons 'return         emit-return     )
    (cons 'comm!          emit-select-comm )
    (cons 'cond!          emit-switch-cond )
    (cons 'cond!*         emit-switch-cond*)
    (cons 'case!          emit-switch-case )
    (cons 'case!*         emit-switch-case*)
    (cons 'type!          emit-switch-type )
    (cons 'type!*         emit-switch-type*)
    (cons 'struct         emit-struct     )
    (cons 'void           emit-void     )
    (cons 'while          emit-for1       )))

(define (droscheme-read-file filename)
  (let ((fnlen (string-length filename)))
    (if (and (> fnlen 4)
             (equal? (substring filename (- fnlen 4) fnlen)
                     ".ild"))
        (sugar-read filename)
        (read filename))))

;; globals

(define *defines* (make-parameter '()))
(define *optionals* (make-parameter '()))
(define *has-export* (make-parameter #f))
(define *emit-function* (make-parameter emit))
(define *input-filename* (make-parameter "/dev/null"))
;(define *syntax-table* (make-parameter default-syntax-table))
(define *package-name* (make-parameter "_"))
(define *package-path* (make-parameter "/"))
(define *libraries* (make-parameter '()))
(define *droscheme-path* (make-parameter (droscheme-path)))
