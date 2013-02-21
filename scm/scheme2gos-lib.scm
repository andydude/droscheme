(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

;; General functions

;; multi-assoc takes an alist and returns all matches
;;
;; For example, if:
;;   alist == '((import x y) (export foo) (import a b c))
;;   obj == 'import
;; then:
;;   (multi-assoc obj alist) = '(x y a b c)
;;
(define (multi-assoc obj alist)
  (let ((apair (assoc obj alist)))
    (if apair
        (apply append (map cdr
         (lset-difference equal? alist
          (alist-delete obj alist))))
        '())))

(define (multi-diff alist . keys)
  (if (null? keys)
      alist
      (alist-delete (car keys)
       (apply multi-diff alist (cdr keys)))))

(define (prefix sym)
  (string->symbol
   (string-append "_" 
    (symbol->string sym))))

;; Compiler logic

(define (compile-clause clause)
  (define (add-return expr . body)
    `(,expr ,(compile-return-block body)))
  (apply add-return clause))

(define (compile-define formals . body)
  (ds-function->gos-define-func
   (define->ds-function
    `(define ,formals ,@body))))

(define (compile-exports . o)
  (define (rename key value)
    ;(display "--rename--\n")
    `(: ,(symbol->string key) ,value))
  (define (per-export spec)
    (if (symbol? spec)
        (list (rename spec spec))
        (if (list? spec)
            (if (eqv? (car spec) 'rename)
                (if (and (= (length spec) 2)
                         (symbol? (cadr spec))
                         (symbol? (caddr spec)))
                    (list (rename (cadr spec) (caddr spec)))
                    (map (lambda (o) (apply rename o)) (cdr spec)))
                (error "export expected rename"))
            (error "export expected symbol or list"))))
  `(func Export () (map: &str &any)
         (return #((map: &str &any) 
                   ,@(apply append (map per-export o))))))

(define (compile-bindings . o)
  (define (list->gos-import-var spec)
    (let* ((name (list->underscore spec))
           (pname (string-append "_" name))
           (sym (string->symbol name))
           (psym (string->symbol pname)))
      `(= ,psym ((dot ,sym Export)))))
  (define (list->gos-import libspec)
    (cons (list->gos-import-var libspec)
          (map ds-function->gos-import 
               (ds-library-defines 
                (list->ds-library libspec)))))
  (define (per-import spec)
    (cond
     ((string? spec) '())
     ((list? spec)
      (cond
       ((eqv? (car spec) 'only)
        (list->gos-import (cadr spec)))
       (else
        (list->gos-import spec))))))
  (cons 'var (apply append (map per-import o))))

(define (compile-imports . o)
  (define (compile-import-spec) "")
  (define (compile-import-vars) "")
  (define (per-binding spec) "")
  (define (per-import-only spec)
    (list->directory (car spec)))
  (define (per-import spec)
    (cond
     ((string? spec) spec)
     ((symbol? spec) spec) ; TODO: racket scheme/base
     ((list? spec)
      (if (memv (car spec)
                '(except
                  only
                  prefix
                  remove-prefix
                  rename))
          (per-import (cadr spec)) ; TODO: processing
          (list->directory spec)))))
  `(import ,@(map per-import o)))

(define (compile-library-6 name . o)
  (define (per-name spec)
    (cond
     ((symbol? spec) spec)
     ((list? spec) (list->underscore-symbol spec))))
  (let ((exports (multi-assoc 'export o))
        (imports (multi-assoc 'import o))
        (defines (multi-diff o 'export 'import)))
    `(package ,(per-name name)
              ,(apply compile-imports imports)
              ,(apply compile-bindings imports)
              ,(apply compile-exports exports)
              ,@(map compile defines))))

(define (compile-library-7 name . o)
  (define (per-name spec)
    (cond
     ((symbol? spec) spec)
     ((list? spec) (list->underscore-symbol spec))))
  (let ((exports (multi-assoc 'export o))
        (imports (multi-assoc 'import o))
        (defines (multi-assoc 'begin o)))
    `(package ,(per-name name)
              ,(apply compile-imports imports)
              ,(apply compile-bindings imports)
              ,(apply compile-exports exports)
              ,@(map compile defines))))

(define (compile-return-block block)
  (list+ (most block) (cons 'return (last block))))

(define (compile-symbol expr)
  expr)

(define (compile-vector expr)
  expr)

(define (compile expr)
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
    (let ((m (apply *rules* expr)))
      (cond ((list? m) m)
            ((string? m) m)
            (else #f)))) ;(error "apply-go unexpected" expr m)))))
  (define (yes-match key . args) (error "yes-match" args))
  (define (no-match key . args) #f);(error "no-match" args))
  (let ((t (catch 'match-error do-match no-match)))
  ;(let ((t (catch 'misc-error
  ;                (lambda () (catch 'match-error do-match no-match))
  ;                yes-match)))
    (if t t (join expr))))

(define (*rules* . expr)
 (match expr

  (('case expr . clauses)
   `(('func () &any (case! ,expr
      ,(map compile-clause clauses)))))

  (('cond . clauses)
   `(('func () &any (cond! ,expr
      ,(map compile-clause clauses)))))

  (('define . o)
   (apply compile-define o))

  (('define-library . o)
   (apply compile-library-7 o))

  (('library . o)
   (apply compile-library-6 o))

 );match
);define
