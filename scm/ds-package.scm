;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Classes
;;

; (define-record-type Package
;   make-package
;   package?
;   (name)    	-- PackageName
;   (imports) 	-- Set of PackageImport
;   (exports) 	-- Set of PackageExport
;   (defines) 	-- Set of Function
;   (body) 		-- BodyExpr -- func init()?
;   (meta)    	-- Hashtable
; )
(define (make-ds-package name imports exports defines body . meta)
  `#(droscheme-package ,name ,imports ,exports ,defines ,body
      ,(list->eqv-hashtable meta)))

(define (ds-package? pg)
  (eqv? (vector-ref pg 0) 'droscheme-package))
(define (ds-package-name pg)
  (unless (ds-package? pg)
          (error "TypeError: in ds-package-name" pg))
  (vector-ref pg 1))
(define (ds-package-imports pg)
  (unless (ds-package? pg)
          (error "TypeError: in ds-package-imports" pg))
  (vector-ref pg 2))
(define (ds-package-export-symbols pg)
  (unless (ds-package? pg)
          (error "TypeError: in ds-package-export-symbols" pg))
  (vector-ref pg 3))
(define (ds-package-defines pg)
  (unless (ds-package? pg)
          (error "TypeError: in ds-package-defines" pg))
  (vector-ref pg 4))
(define (ds-package-body pg)
  (unless (ds-package? pg)
          (error "TypeError: in ds-package-body" pg))
  (vector-ref pg 5))
(define (ds-package-meta pg)
  (unless (ds-package? pg)
          (error "TypeError: in ds-package-meta" pg))
  (vector-ref pg 6))

(define (ds-package-exports pg)
  ;(write pg)
  (let* ((defines (ds-package-defines pg))
         (exports (ds-package-export-symbols pg)))
    (define (pred pe)
      (if pe
          (member (ds-package-export-name pe) exports)
          #f))
    (filter pred (map ds-function-export (filter values defines)))))

; (define-record-type PackageName
;   make-package-name
;   package-name?
;   (list)   	-- List of (Symbol | Integer) i.e.: (scheme base)
;   (filename)  -- String i.e.: "scheme_base"
;   (filepath)  -- String i.e.: "scheme/base"
;   (path) 		-- String
; )
(define (make-ds-package-name list filename filepath path)
  `#(droscheme-package-name ,list ,filename ,filepath ,path))

(define (ds-package-name? pn)
  (eqv? (vector-ref pn 0) 'droscheme-package-name))
(define (ds-package-name-list pn)
  (unless (ds-package-name? pi)
          (error "TypeError: in ds-package-name-list" pn))
  (vector-ref pn 1))
(define (ds-package-name-filename pn)
  (unless (ds-package-name? pn)
          (error "TypeError: in ds-package-name-filename" pn))
  (vector-ref pn 2))
(define (ds-package-name-filepath pn)
  (unless (ds-package-name? pn)
          (error "TypeError: in ds-package-name-filepath" pn))
  (vector-ref pn 3))
(define (ds-package-name-path pn)
  (unless (ds-package-name? pn)
          (error "TypeError: in ds-package-name-path" pn))
  (vector-ref pn 4))

(define (ds-package-name->ds-package pn)
  (let* ((path (ds-package-name-path pn)))
    (if (file-exists? path)
        (->ds-package (call-with-input-file path read))
        #f)))

(define (list->ds-package list)
  (ds-package-name->ds-package
   (->ds-package-name list)))


; (define-record-type PackageImport
;   make-package-import
;   package-import?
;   (package) 	-- PackageName
;   (symbols) 	-- Set of PackageExport
;   (meta)    	-- Hashtable
; )
(define (make-ds-package-import package symbols . meta)
  `#(droscheme-package-import ,package ,symbols
      ,(list->eqv-hashtable meta)))

(define (ds-package-import? pi)
  (eqv? (vector-ref pi 0) 'droscheme-package-import))
(define (ds-package-import-package pi)
  (unless (ds-package-import? pi)
          (error "TypeError: in ds-package-import-package" pi))
  (vector-ref pi 1))
(define (ds-package-import-symbols pi)
  (unless (ds-package-import? pi)
          (error "TypeError: in ds-package-import-symbols" pi))
  (vector-ref pi 2))
(define (ds-package-import-meta pi)
  (unless (ds-package-import? pi)
          (error "TypeError: in ds-package-import-meta" pi))
  (vector-ref pi 3))

(define (ds-package-import-filename pi)
  (ds-package-name-filename (ds-package-import-package pi)))

(define (ds-package-import-filepath pi)
  (ds-package-name-filepath (ds-package-import-package pi)))

;; returns a list of var specs
(define (ds-package-import->gos pi)
  (let* ((pn (ds-package-import-package pi))
         (fn (ds-package-name-filename pn))
         (un (string->symbol (string-append "_" fn)))
         (symbols (ds-package-import-symbols pi)))
    (define (per-package)
      `(go:= ,un (go:dot ,(string->symbol fn) Export)))
    (define (per-symbol pe)
      (let* ((name (symbol->string (ds-package-export-gos-name pe)))
             (gos-name (ds-package-export-gos-name pe))
             (gos-type (ds-package-export-gos-type pe)))
        `(go:= ,gos-name (go:as (go:index ,un ,name) ,gos-type))))
    (if (null? symbols)
        '()
        (cons (per-package) (map per-symbol symbols)))))

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
;  (cons 'go:var (apply append (map per-import o))))


; (define-record-type PackageExport
;   make-package-export
;   package-export?
;   (name) 		-- Symbol
;   (gos-name)  -- Symbol
;   (gos-type)  -- TypeExpr
;   (package) 	-- PackageName | #f
;   (meta) 		-- Hashtable
; )
(define (make-ds-package-export name gos-name gos-type package . meta)
  `#(droscheme-package-export ,name ,gos-name ,gos-type ,package
      ,(list->eqv-hashtable meta)))

(define (ds-package-export? pe)
  (eqv? (vector-ref pe 0) 'droscheme-package-export))
(define (ds-package-export-name pe)
  (unless (ds-package-export? pe)
          (error "TypeError: in ds-package-export-name" pe))
  (vector-ref pe 1))
(define (ds-package-export-gos-name pe)
  (unless (ds-package-export? pe)
          (error "TypeError: in ds-package-export-gos-name" pe))
  (vector-ref pe 2))
;(define (ds-package-export-gos-name-set! pe gos-name)
;  (unless (ds-package-export? pe)
;          (error "TypeError: in ds-package-export-gos-name" pe))
;  (vector-set! pe 2 gos-name))
(define (ds-package-export-gos-type pe)
  (unless (ds-package-export? pe)
          (error "TypeError: in ds-package-export-gos-type" pe))
  (vector-ref pe 3))
;(define (ds-package-export-gos-type-set! pe gos-type)
;  (unless (ds-package-export? pe)
;          (error "TypeError: in ds-package-export-gos-name" pe))
;  (vector-set! pe 2 gos-type))
(define (ds-package-export-package pe)
  (unless (ds-package-export? pe)
          (error "TypeError: in ds-package-export-package" pe))
  (vector-ref pe 4))
;(define (ds-package-export-package-set! pe package)
;  (unless (ds-package-export? pe)
;          (error "TypeError: in ds-package-export-gos-name" pe))
;  (vector-set! pe 2 package))
(define (ds-package-export-meta pe)
  (unless (ds-package-export? pe)
          (error "TypeError: in ds-package-export-meta" pe))
  (vector-ref pe 5))

(define (ds-package-export->gos pe)
  (let* ((value (ds-package-export-name pe))
         (key (symbol->string value)))
    `(go:: ,key ,value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Methods
;;

;; requires
;;  (multi-assoc alist key)
;;  (multi-diff alist . keys)
(define (->ds-package expr)
  (when (not (pair? expr))
        (error "TypeError in ->ds-package"))
  (cond

   ((eqv? (car expr) 'library)
    (let* ((body (cdr expr))
           (name (car body))
           (body (cdr body)))
      (make-ds-package
       (->ds-package-name name)
       (map ->ds-package-import (multi-assoc body 'import))
       (map ->ds-package-export (multi-assoc body 'export))
       (map ->ds-package-define (multi-diff body 'export 'import))
       (multi-diff body 'import 'export 'define)
       '(features r6rs))))

   ((eqv? (car expr) 'define-library)
    (let* ((body (cdr expr))
           (name (car body))
           (body (cdr body)))
      (make-ds-package
       (->ds-package-name name)
       (map ->ds-package-import (multi-assoc body 'import))
       (map ->ds-package-export (multi-assoc body 'export))
       (map ->ds-package-define (multi-assoc body 'begin))
       (multi-diff (multi-assoc body 'begin) 'define)
       '(features r7rs))))

   (else #f)))

;; TOOD: move these to ds-common
(define list->filepath list->directory)
(define list->filename list->underscore)
(define (filepath->list filepath)
  (map string->symbol (string-split filepath #\/)))

(define (->ds-package-name expr)
  (cond
   ((string? expr)
    (let* ((list (filepath->list expr))
           (filename (list->filename list)))
      (make-ds-package-name list filename expr #f)))
   ((list? expr)
    (let ((filepath (list->filepath expr))
          (filename (list->filename expr)))
      (make-ds-package-name expr filename filepath
        (string-append (droscheme-path) "/src/" filepath "/" filename ".sld"))))))

(define (->ds-package-export expr)
  expr)
;  (let ((fn (->ds-function expr)))
;    (if fn
;        (ds-function-export fn)
;        #f)))

(define (->ds-package-import expr)
  (cond
   ((not expr) #f)
   ((string? expr)
    (make-ds-package-import (->ds-package-name expr) '()))
   ((list? expr)
    (match expr
     (('except spec . symbols)
      (apply ds-package-import-except (->ds-package-import spec) symbols))
     (('only spec . symbols)
      (apply ds-package-import-only (->ds-package-import spec) symbols))
     (('prefix spec prefix)
      (ds-package-import-prefix (->ds-package-import spec) prefix))
     (('remove-prefix spec prefix)
      (ds-package-import-remove-prefix (->ds-package-import spec) prefix))
     (('rename spec . symbols)
      (apply ds-package-import-rename (->ds-package-import spec) symbols))
     (else
      (let* ((pn (->ds-package-name expr))
             (pg (ds-package-name->ds-package pn)) ; IO
             (pe (ds-package-exports pg)))
        (make-ds-package-import pn pe)))))))

;; TODO
(define (ds-package-import-except pi . symbols)
  pi)

(define (ds-package-import-only pi . symbols)
  pi)

(define (ds-package-import-prefix pi prefix)
  pi)

(define (ds-package-import-remove-prefix pi prefix)
  pi)

(define (ds-package-import-rename pi . clauses)
  pi)


;; if it's not a function, we don't care
(define (->ds-package-define expr)
  (->ds-function expr))

;         (name (list->underscore spec))
;         (filename (string-append (droscheme-path) "/src/" path "/" name ".sld"))
;         (expr (call-with-input-file filename read))
;         (libname (cadr expr))
;         (exports (assoc 'export (cdr expr)))
;         (imports (assoc 'import (cdr expr)))
;         (defines (assoc 'begin (cdr expr))))
;  (let ((exports (multi-assoc 'export o))
;        (imports (multi-assoc 'import o))
;    `(go:package ,(per-name name)
;              ,(apply compile-imports imports)
;              ,(apply compile-bindings imports)
;              ,(apply compile-exports exports)
;              ,@(map compile defines))))
; (apply compile-this expr))
;
;  (let ((exports (multi-assoc 'export o))
;        (imports (multi-assoc 'import o))
;        (defines (multi-assoc 'begin o)))
;    `(go:package ,(per-name name)
;              ,(apply compile-imports imports)
;              ,(apply compile-bindings imports)
;              ,(apply compile-exports exports)
;              ,@(map compile defines))))



;; MOVED UP
;(define (load-ds-package spec)
;(define (list->ds-package spec)
;  (define (library-keyword? spec)
;    (and (list? spec)
;         (memv (car spec)
;               '(except
;                 only
;                 prefix
;                 remove-prefix
;               rename))))
;  (define (list->library-name spec)
;    (if (library-keyword? spec)
;        (cadr spec)
;        spec))
;  (define (ds-package-export-imports libr)
;    (write (ds-package-name libr))
;    (write (ds-package-imports libr))
;    (newline)
;    (ds-package-imports libr))
;  (define (ds-package-export-defines libr)
;    (define (exported? es)
;      (lambda (def) (member (ds-function-name def) es)))
;    (let* ((ds (ds-package-defines libr))
;           (es (ds-package-export-symbols libr)))
;      (filter (exported? es) ds)))
;  (define (decl-import spec)
;    (ds-package-export-imports (list->ds-package (list->library-name spec))))
;  (define (decl-imports specs)
;    (apply append (map decl-import specs)))
;  (define (decl-define spec)
;    (ds-package-export-defines (list->ds-package (list->library-name spec))))
;  (define (decl-defines specs)
;    (apply append (map decl-define specs)))
;  (define (decl->func decl)
;    (let ((f (let ((kind (car decl)))
;      (cond
;       ((eqv? kind 'define)
;        (define->ds-function decl))
;       ((eqv? kind 'define-func)
;        (define-func->ds-function decl))
;       (else #f)))))
;      (ds-function-attributes-set! f 'library-name spec)
;      f))
;  ;(display "dl")
;  ;(write spec)
;  ;(newline)
;  (parameterize ((*package-path* (list->directory spec))
;                 (*package-name* (list->underscore spec)))
;  ;(write (*package-name*))
;  ;(write (*package-path*))
;  (let* ((path (list->directory spec))
;         (name (list->underscore spec))
;         (filename (string-append (droscheme-path) "/src/" path "/" name ".sld"))
;         (expr (call-with-input-file filename read))
;         (libname (cadr expr))
;         (exports (assoc 'export (cdr expr)))
;         (imports (assoc 'import (cdr expr)))
;         (defines (assoc 'begin (cdr expr))))
;    (make-ds-package (->ds-package-name libname)
;      (if exports (cdr exports) '())
;      (if imports
;          (append (decl-imports (cdr imports))
;                  (if (not imports)
;                      '()
;                      (cdr imports)))
;          '())
;      (if imports
;          (append (decl-defines (cdr imports))
;                  (if (not defines)
;                      '()
;                      (map decl->func (cdr defines))))
;          (if (not defines)
;              '()
;              (map decl->func (cdr defines))))))))

;(define (ds-package->gos-import-spec libr)
;  (let* ((name (ds-package-name libr)))
;    (ds-package-name-path name)))
;
;(define (ds-package->gos-import-vars libr)
;  (let* ((symbols (ds-package-symbols libr)))
;    (map ds-package-symbol->gos-import symbols)))
;
;(define (ds-package-symbol->gos-export pe))
;
;(define (ds-package->gos-export-pairs libr)
;  (let* ((symbols (ds-package-symbols libr)))
;    (map ds-package-symbol->gos-export symbols)))

;(define (ds-package->go-import libr)
;  (define (decl->func decl)
;    (let ((kind (car decl)))
;      (cond
;       ((eqv? kind 'define)
;        (define->ds-function decl))
;       ((eqv? kind 'define-func)
;        (define-func->ds-function decl))
;       (else #f))))
;  (let* ((funcs (ds-package-symbols libr))
;         (decls (map ds-function->go-import funcs)))
;    (string-join decls "\n")))
