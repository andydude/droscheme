(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define (prefix sym)
  (string->symbol
   (string-append "_" 
    (symbol->string sym))))

;; Compiler logic
;; I need something that takes  (a . b)
;; and returns (cons a (f b))
;;
;; (apply-to-cdr proc pair)
;; (apply-to-car+cdr procar procdr pair)
;; (apply-to-last proc list)
;; (compile-return-block body)
;; (compile-return-stmt stmt)

(define (compile-let->forms binds)
  (map car binds))

(define (compile-let->inits binds)
  (map cadr binds))

(define (compile-let->vars binds)
  (define (pair->var form init)
    `(go::= ,form ,init))
  (define (list->var ls)
    (let ((f (car ls))
          (i (cadr ls)))
      (pair->var f i)))
  (map list->var binds))

(define (compile-condition expr)
  `(go:as ,(compile expr) go:bool))

(define (compile-cond-clause clause)
  (define (cond-return expr . body)
    (cons
     (compile-condition expr)
     (compile-return-block body)))
  (define (else-return else . body)
    (compile-return-block body))
  (if (and (pair? clause) (eqv? (car clause) 'else))
      (cons 'go:else (cdr (apply else-return clause)))
      (apply cond-return clause)))

(define (compile-case-clause var)
 (lambda (clause)
  (define (case-return lits . body)
    (define (case->cond lit)
      `(eqv? ,var ,lit))
    (define (test-literal expr)
      `(,(case->cond expr) (go:fallthrough)))
    (list+ (map test-literal (most lits))
           `(,(case->cond (last lits)) ,@body)))
  (if (and (pair? clause) (eqv? (car clause) 'else))
      (list clause)
      (apply case-return clause))))

(define (compile-default-parameters func name formals ret . body)
  (define (suffix sym) (if (symbol? sym) (string->symbol (string-append (symbol->string sym) "_")) sym))
  (define (snoc a b) (cons b a))
  (define ls '())
  (define (parameter formal nm)
    (let* ((ca (car formal))
           (cd (cdr formal))
           (sf (suffix ca)))
      (set! ls (snoc ls `(go::= ,ca (if (go:< (go:len Rest) ,nm) (go:index Rest ,nm) ,cd))))
      'go:void))
  (define (default-params formals nm)
    (if (pair? formals)
        (let ((formal (car formals))
              (rest (cdr formals)))
          (if (pair? formal)
              (begin (parameter formal nm)
                     (default-params rest (+ nm 1))
                     '())
              (cons formal (default-params rest 0))))
        'Rest))
  (define (parameters formals)
    (if (pair? formals)
        (let ((formal (car formals))
              (rest (cdr formals)))
          (if (pair? formal)
              (default-params formals 0)
              (cons formal (parameters rest))))
        formals))
  (display "--comdefparams\n")
  (write `(,func ,name ,formals ,ret ,@body))
  (if (any pair? formals)
      `(go:func... ,name ,(parameters formals) ,ret ,@(cons ls body))
      `(,func ,name ,formals ,ret ,@body)))

(define (compile-function expr)
  (compile-ds-function (->ds-function expr)))
         
(define (compile-ds-function fn)
  (let* ((head (ds-function->gos-func-head fn))
         (body (ds-function-body fn)))
    (append head (compile-return-block body))))

(define (compile-package expr)
  (define (compile-imports imports)
    `(go:import ,@(map ds-package-import-filename imports)))
  (define (compile-import-vars imports)
    `(go:var ,@(apply append (map ds-package-import->gos imports))))
  (define (compile-exports exports)
    `(go:func Export () go:internal:frame
       (go:return #(go:internal:frame
         ,@(map ds-package-export->gos exports)))))

  (let* ((pg (->ds-package expr))
         (name (string->symbol (ds-package-name-filename (ds-package-name pg))))
         (imports (ds-package-imports pg))
         (exports (ds-package-exports pg))
         (defines (ds-package-defines pg))
         (mode (droscheme-compile-mode))
         (body (ds-package-body pg)))
    ;(write `(compile-package ,expr ,mode))
    (cond

     ((eqv? mode 'import)
      `(go:package ,name
                   ,(compile-imports imports)
                   ,(compile-import-vars imports)))

     ((eqv? mode 'export)
      `(go:package ,name
                   ,(compile-imports imports)
                   ,(compile-exports exports)))

     ((eqv? mode 'library)
      `(go:package ,name
                   ,(compile-imports imports)
                   ,@(map compile-ds-function defines)))
                   ;,(compile-init body)))

     ((eqv? mode 'program)
      `(go:package main
                   ,(compile-imports imports)
                   ,@(map compile-ds-function defines)
                   ,(compile-main body)))
     (else #f))))

;(define (compile-exports . o)
;  (define (rename key value)
;    ;(display "--rename--\n")
;    `(go:: ,(symbol->string key) ,value))
;  (define (per-export spec)
;    ;(write `(per-import ,spec))
;    (if (symbol? spec)
;        (list (rename spec spec))
;        (if (list? spec)
;            (if (eqv? (car spec) 'rename)
;                (if (and (= (length spec) 2)
;                         (symbol? (cadr spec))
;                         (symbol? (caddr spec)))
;                    (list (rename (cadr spec) (caddr spec)))
;                    (map (lambda (o) (apply rename o)) (cdr spec)))
;                (error "export expected rename"))
;            (error "export expected symbol or list"))))
;(define (compile-import-vars . o)
;  (define (list->gos-import-var spec)
;    (let* ((name (list->underscore spec))
;           (pname (string-append "_" name))
;           (sym (string->symbol name))
;           (psym (string->symbol pname)))
;      `(= ,psym ((dot ,sym Export)))))
;  (define (list->gos-import libspec)
;    (cons (list->gos-import-var libspec)
;          (map ds-function->gos-import 
;               (ds-library-defines 
;                (list->ds-library libspec)))))
;  (define (per-import spec)
;    ;(write `(per-import ,spec))
;    (cond
;     ((string? spec) '())
;     ((list? spec)
;      (cond
;       ((eqv? (car spec) 'only)
;        (list->gos-import (cadr spec)))
;       (else
;        (list->gos-import spec))))))
;  (cons 'go:var (apply append (map ds-package-import->gos o))))
;
;(define (compile-import-spec . o)
;  (define (per-import pi)
;    (ds-package-import-filename pi))
;  `(go:import ,@(map per-import o)))

(define (compile-main body)
  #f)



;(define (compile-package-6 expr)
; (define (compile-this name . o)
;  (define (per-name spec)
;    (cond
;     ((symbol? spec) spec)
;     ((list? spec) (list->underscore-symbol spec))))
;  ;(write `(compile-package-6 ,name))
;  (let ((exports (multi-assoc 'export o))
;        (imports (multi-assoc 'import o))
;        (defines (multi-diff o 'export 'import)))
;    `(go:package ,(per-name name)
;              ,(apply compile-import-spec imports)
;              ,(apply compile-import-vars imports)
;              ,(apply compile-exports exports)
;              ,@(map compile defines))))
; (apply compile-this expr))
;
;(define (compile-package-7 expr)
; (define (compile-this name . o)
;  (define (per-name spec)
;    (cond
;     ((symbol? spec) spec)
;     ((list? spec) (list->underscore-symbol spec))))
;  ;(write `(compile-package-7 ,name))
;  (let ((exports (multi-assoc 'export o))
;        (imports (multi-assoc 'import o))
;        (defines (multi-assoc 'begin o)))
;    `(go:package ,(per-name name)
;              ,(apply compile-import-spec imports)
;              ,(apply compile-import-vars imports)
;              ,(apply compile-exports exports)
;              ,@(map compile defines))))
; (apply compile-this expr))

(define (compile-return-block block)
  (define (append-return lb)
    (if (and (pair? lb)
             (or (eqv? (car lb) 'go:return)
                 (eqv? (car lb) 'go:fallthrough)
                 (eqv? (car lb) 'go:continue)
                 (eqv? (car lb) 'go:break)))
        lb
        `(go:return ,lb)))
  (list+ (map compile (most block)) 
         (append-return (compile (last block)))))

(define (compile-symbol expr)
  expr)

(define (compile-vector expr)
  expr)

(define (compile expr)
  ;(write `(compile ,expr))
  (cond
    ((list? expr) (compile-list expr))
    ((pair? expr) (error "unexpected list*"))
    ((symbol? expr) (compile-symbol expr))
    ((vector? expr) (compile-vector expr))
    ((boolean? expr) expr)
    ((char? expr) expr)
    ((null? expr) expr)
    ((number? expr) expr)
    ((string? expr) expr)
    (else (error "emit unrecognized type"))))

(define (compile-list expr)
  (define (join rest)
    (map compile rest))
  (define (do-match)
    (if (list? expr)
        (let ((m (*rules* expr)))
          (cond ((list? m) m)
                ((string? m) m)
                (else #f))) ;(error "apply-go unexpected" expr m)))))
        expr))
  (define (yes-match key . args) (error "yes-match" args))
  (define (no-match key . args) #f);(error "no-match" args))
  (let ((t (catch 'match-error do-match no-match)))
  ;(let ((t (catch 'misc-error
  ;                (lambda () (catch 'match-error do-match no-match))
  ;                yes-match)))
    (if t t (join expr))))

(define (*rules* expr)
 (match expr

  (('and) #t)
  (('and a) a)
  (('and a . o)
   (compile-list `(if ,a (and ,@o) #f)))

  (('or) #f)
  (('or a) a)
  (('or a . o)
   (compile-list `(if ,a ,a (or ,@o))))

  (('if cond then else)
   `((go:func () go:any
      (go:when (go:as ,(compile cond) go:bool)
               (go:return ,(compile then)))
      (go:return ,(compile else)))))

  (('if cond then)
   (compile-list `(if ,cond ,then (void))))

  (('case expr . clauses)
   (compile-list `(cond ,@(apply append
    (map (compile-case-clause expr) clauses)))))

  (('cond . clauses)
   `((go:func () go:any (go:cond!
      ,@(map compile-cond-clause clauses))
      (go:return (void)))))

  (('when . o)
   `(go:when ,@o))

  (('when* . o)
   `(go:when* ,@o))

  (('unless . o)
   `(go:unless ,@o))

  (('unless* . o)
   `(go:unless* ,@o))

  ;(('cond-expand . o))
  ;(('import . o))
  ;(('export . o))

  (('begin . o)
   `((go:func () go:any
      ,@(compile-return-block o))))

  (('let* binds . o)
   (compile-list
    (let ((vs (compile-let->vars binds)))
      `(begin ,@vs ,@o))))

  (('let (? list? binds) . o)
   (compile-list 
    (let ((fs (compile-let->forms binds))
          (is (compile-let->inits binds)))
      `((lambda ,fs ,@o) ,@is))))

  (('let (? symbol? name) binds . o)
   (compile-list 
    (let ((fs (compile-let->forms binds))
          (is (compile-let->inits binds)))
      `(begin
         (define (,name ,@fs) ,@o)
         (,name ,@is)))))
   
  (('define! (? symbol? x) y)
   `(go::= ,x ,y))
   
  (('set! (? symbol? x) y)
   `(go:= ,x ,y))

  (('define . o)
   (compile-function expr))

  (('lambda . o)
   (compile-function expr))

  (('thunk . o)
   (compile-function expr))

  (('define-library . o)
   (compile-package expr))

  (('library . o)
   (compile-package expr))

 );match
);define
