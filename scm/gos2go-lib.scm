; -*- mode: scheme -*-
(use-modules (ice-9  match))

(define *top-context* (make-parameter #t))
(define *type-context* (make-parameter #f))
(define *binary-context* (make-parameter #f))

;; General functions

(define (void)
  (if #f #f))

(define (void? obj)
  (unspecified? obj))

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

(define (go-encoded? name)
  (if (>= (string-length name) 2)
      (let ((c0 (string-ref name 0))
            (c1 (string-ref name 1)))
        (and (eqv? c0 #\Z) (not (eqv? c1 #\Z))))
      #f))

;(define (go-public? name)
;  (ds-function-name-public? name))

(define (go-keyword? kw)
  (define *table* '(
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
  (memv (string->symbol kw) *table*))


(define (go-prefixed-keyword? kw)
  (define *table* '(
    go:append
    go:bool
    go:break
    go:byte
    go:cap
    go:case
    go:chan
    go:close
    go:complex
    go:complex128
    go:complex64
    go:const
    go:continue
    go:copy
    go:default
    go:defer
    go:delete
    go:else
    go:error
    go:fallthrough
    go:false
    go:float32
    go:float64
    go:for
    go:func
    go:go
    go:goto
    go:if
    go:imag
    go:import
    go:int
    go:int16
    go:int32
    go:int64
    go:int8
    go:interface
    go:iota
    go:len
    go:make
    go:map
    go:new
    go:nil
    go:package
    go:panic
    go:print
    go:println
    go:range
    go:real
    go:recover
    go:return
    go:rune
    go:select
    go:string
    go:struct
    go:switch
    go:true
    go:type
    go:uint
    go:uint16
    go:uint32
    go:uint64
    go:uint8
    go:uintptr
    go:var))
  (memv (string->symbol kw) *table*))

;; Emit functions

(define (emit-char ch)
  (string-append "'" (string-escape (string ch)) "'"))

(define (emit-string s)
  (string-append "\"" (string-escape s) "\""))

;(define (emit-symbol id)
;  (symbol->string id))
(define (emit-symbol id)
  (define *table* '(
          ; my types
          ("go:any" . "interface{}")
          ("go:internal:env" . "*ds_env.Env")
          ("go:internal:frame" . "map[string]interface{}")
          ("go:internal:table" . "map[uintptr][]interface{}")
          ("go:internal:value" . "reflect.Value") ; required by (prim-apply)
          ("go:internal:values" . "[]reflect.Value") ; required by (prim-apply)
          ("go:internal:vector" . "[]interface{}")   ; required by Apply()
          ("go:internal:string" . "[]rune")
          ("go:internal:binary" . "[]byte")
          ("go:type" . "type")

          ; go types
          ("go:bool" . "bool")
          ("go:byte" . "byte")
          ("go:complex64" . "complex64")
          ("go:complex128" . "complex128")
          ("go:error" . "error")
          ("go:float32" . "float32")
          ("go:float64" . "float64")
          ("go:int" . "int")
          ("go:int8" . "int8")
          ("go:int16" . "int16")
          ("go:int32" . "int32")
          ("go:int64" . "int64")
          ("go:rune" . "rune")
          ("go:string" . "string")
          ("go:uint" . "uint")
          ("go:uint8" . "uint8")
          ("go:uint16" . "uint16")
          ("go:uint32" . "uint32")
          ("go:uint64" . "uint64")
          ("go:uintptr" . "uintptr")
          ("go:void" . "")
          ; builtins
          ("go:append" . "append")
          ("go:cap" . "cap")
          ("go:close" . "close")
          ("go:complex" . "complex")
          ("go:copy" . "copy")
          ("go:delete" . "delete")
          ("go:imag" . "imag")
          ("go:len" . "len")
          ("go:panic" . "panic")
          ("go:print" . "print")
          ("go:println" . "println")
          ("go:real" . "real")
          ("go:recover" . "recover")
          ("go:make" . "make")
          ("go:new" . "new")
          ; objects
          ("go:nil" . "nil")))
  (if (symbol? id)
      (let ((s (symbol->string id)))
        (if (assoc s *table*)
            (cdr (assoc s *table*))
            (if (go-keyword? s)
                (string-append "__" s)
                (symbol->mangle id))))
      (error id)))

(define (emit-op sym)
  (define (goop->op goop) (list->string (cdddr (string->list goop))))
  (define *table* '(
          ("go:bitwise-and" . "&")
          ("go:bitwise-and=" . "&=")
          ("go:bitwise-but" . "&^")
          ("go:bitwise-but=" . "&^=")
          ("go:bitwise-or" . "|")
          ("go:bitwise-or=" . "|=")
          ("go:bitwise-xor" . "^")
          ("go:bitwise-xor=" . "^=")
          ("go:and" . "&&")
          ("go:dot" . ".")
          ("go:not" . "!")
          ("go:or" . "||")))
  (cond
   ((string? sym) sym)
   ((symbol? sym)
    (let ((str (symbol->string sym)))
      (if (assoc str *table*)
          (cdr (assoc str *table*))
          (goop->op str))))
   (else (error "emit-op expected string or symbol, got" sym))))

(define (emit-literal lit)
  (string-append 
   (emit-type (car lit)) "{" 
   (emit-exprs (cdr lit)) "}"))

(define (emit-params fields)
  (parameterize ((*type-context* #t))
   (string-join (map emit-field fields) ", ")))

(define (emit-params... fields)
  (display "**DEPRECATED**")
  (parameterize ((*type-context* #t))
   (string-join (append 
    (map emit-field (most fields)) 
    (list (emit-field... (last fields)))) ", ")))

(define (emit-fields fields)
  (string-join (map emit-field fields) "\n"))

(define (emit-field field)
  (if (vector? field)
      (let ((ms (reverse (cdr (reverse (vector->list field)))))
            (ls (vector-last field)))
        (string-append (emit-exprs ms) " " (emit-expr ls)))
      (if (and (list? field)
               (eqv? (car field) 'go:func)
               (symbol? (cadr field)))
          (emit-method field)
          (emit-expr field))))

(define (emit-field... field)
  (display "**DEPRECATED**")
  (if (vector? field)
      (let ((ms (reverse (cdr (reverse (vector->list field)))))
            (ls (vector-last field)))
        (string-append (emit-exprs ms) " ..." (emit-expr ls)))
      (string-append " ..." (emit-expr field))))

(define (emit-method decl)
  (let* ((name (cadr decl))
         (sig (caddr decl))
         (ret (cadddr decl)))
    (string-append (emit-symbol name) " " 
                   (emit-sig sig ret))))

(define (emit-branch kwsym rest)
  (let ((kw (symbol->string kwsym)))
    (if (null? rest)
        kw
        (string-append kw " " (emit-expr (car rest))))))

(define (emit-assign op vars . vals)
  ;(write (list 'emit-assign op vars vals))
  (string-append
    (cond
      ;; string means cached output
      ((string? vars) vars)
      ((symbol? vars) (emit-symbol vars))
      ((vector? vars) (emit-field vars))
      ((list? vars) (emit-exprs vars))
      (else (error "assign-stmt expected list or symbol" vars)))
    " "
    (emit-op op)
    " "
    (emit-exprs vals)))

(define (emit-binary op . vals)
  (define (mul-op? op) (memv op '(* / % << >> bitwise-and bitwise-but)))
  (define (add-op? op) (memv op '(+ - bitwise-or bitwise-xor)))
  (define (rel-op? op) (memv op '(== != < <= > >=)))
  (define (and-op? op) (eqv? op 'and))
  (define (or-op? op) (eqv? op 'or))
  (define (op<=? op qo)
    (cond
     ((or-op? op) #t)
     ((and-op? op) (not (or-op? qo)))
     ((rel-op? op) (and (not (or-op? qo)) (not (and-op? qo))))
     ((add-op? op) (or (add-op? qo) (mul-op? qo)))
     ((mul-op? op) (mul-op? qo))))
  (define (emit1 expr)
    ;; simple precedence rules
    (if (droscheme-precedence)
        (if (and (pair? expr)
                 (op<=? op (car expr)))
            (emit-expr expr)
            (parameterize
             ((*binary-context* #t))
             (emit-expr expr)))
        (emit-expr expr)))
  (define (emit2 opstr)
    (if (= (length vals) 1)
        (string-append opstr (emit-expr (car vals)))
        (string-join (map emit1 vals)
                     (string-append " " opstr " "))))
  (if (droscheme-precedence)
      (if (*binary-context*)
          (string-append "(" (emit2 (emit-op op)) ")")
          (emit2 (emit-op op)))
      (string-append "(" (emit2 (emit-op op)) ")")))

(define (emit-parens kw proc rest)
  ;(display "\nemit-parens")
  ;(write rest)
  (if (= (length rest) 1)
      (string-append kw " " (proc rest) "\n")
      (string-append kw " (\n" (proc rest) "\n)\n")))

(define (emit-imports specs)
  (define (emit-spec spec)
    ;(display "\nemit2")
    ;(write spec)
    ;(newline)
    (cond
     ((string? spec) (emit-string spec))
     ((pair? spec)
      (cond
       ((eqv? (car spec) 'as)
        (string-append (emit-symbol (cadr spec)) " " (emit-string (caddr spec))))
       ((eqv? (car spec) 'dot)
        (string-append ". " (emit-string (cadr spec))))
       (else (error "import expected list or string"))))))
  ;(write specs)
  (string-join (map emit-spec specs) "\n"))

(define (emit-interface-block types)
  (if (= (length types) 0)
      "{}"
      (string-append "{\n" (emit-fields types) "\n}\n")))

(define (emit-type-block types)
  (if (= (length types) 0)
      "{}"
      (string-append "{\n" (emit-fields types) "\n}\n")))

(define (emit-type-parens kw proc rest)
  (if (= (length rest) 2)
      (string-append kw " " (proc rest) "\n")
      (string-append kw " (\n\t" (proc rest) "\n)\n")))

(define (emit-types specs)
  (define emit1 emit-expr)
  (define (emit2 name spec . specs)
    (let ((s (string-append (emit1 name) " " (emit1 spec))))
      (if (null? specs)
          (list s)
          (cons s (apply emit2 specs)))))
  (string-join (apply emit2 specs) "\n"))

(define (emit-values specs)
  (define (emit spec)
    (if (and (list? spec) (eqv? (car spec) '=))
        (apply emit-assign spec)
        (emit-field spec)))
  (string-join (map emit specs) "\n"))

(define (emit-cases clauses)
  (define (emit-case rest)
    (let ((exprs (car rest))
          (stmts (cdr rest)))
      (if (eqv? exprs 'go:else)
          (string-append "default:\n" (emit-stmts stmts))
          (string-append "case " (emit-exprs exprs) ":\n" (emit-stmts stmts)))))
  (string-append "{\n" (string-join (map emit-case clauses) "\n") "\n}\n"))

(define (emit-conds clauses)
  (define (emit-cond rest)
    (let ((expr (car rest))
          (stmts (cdr rest)))
      (if (eqv? expr 'go:else)
          (string-append "default:\n" (emit-stmts stmts))
          (string-append "case " (emit-expr expr) ":\n" (emit-stmts stmts)))))
  (string-append "{\n" (string-join (map emit-cond clauses) "\n") "\n}\n"))

(define (emit-range a)
  (define (emit sym lhs . rhs)
    (if (eqv? sym 'go::=)
        (apply emit-assign ":= range " lhs rhs)
        (error "emit-range expected :=, got" sym)))
  (apply emit a))

(define (emit-return-type ret)
  (define (emit-return-values ret)
    (string-append "(" (emit-params ret) ")"))
  (if (vector? ret)
      (string-append "(" (emit-field ret) ")")
      (if (and (list? ret)
               (eq? (car ret) 'values))
          (emit-return-values (cdr ret))
          (emit-field ret))))

(define (emit-sig ins ret)
  (string-append "(" 
    (emit-params ins) ")" 
    (emit-return-type ret)))

(define (emit-sig... ins ret)
  (display "**DEPRECATED**")
  (string-append "(" 
    (emit-params... ins) ")"
    (emit-return-type ret)))

(define (emit-type t)
  (parameterize ((*type-context* #t))
                (emit-expr t)))

(define (emit-as a b . rest)
  (if (null? rest)
      (string-append (emit-expr a) ".(" (emit-type b) ")")
      (apply emit-expr 'dot (list 'as a b) rest)))

(define (emit-else-block stmts)
  (if (null? stmts)
      "{}"
      (let ((ms (most stmts))
            (ls (last stmts)))
        (if (eqv? (car ls) 'go:else)
            ;(if (= (length ls) 2)
            ;    ;; else if
            ;    (string-append "{\n\t" 
            ;     (emit-stmts ms) "\n} else " 
            ;     (emit-stmts (cdr ls)))
                ;; else
                (string-append "{\n\t" 
                 (emit-stmts ms) "\n} else {\n" 
                 (emit-stmts (cdr ls)) "\n}\n")
            (emit-block stmts)))))

(define (emit-block stmts)
 (parameterize ((*top-context* #f))
  (if (null? stmts)
      ""
      (string-append "{\n\t" (emit-stmts stmts) "\n}"))))

(define (emit-decls decls)
  (define (emit decl)
    (emit-expr decl))
  (string-join (map emit decls) "\n\n"))

(define (emit-stmts stmts)
  (define (emit stmt)
    (emit-expr stmt))
  (string-join (map emit stmts) "\n"))

(define (emit-exprs exprs)
  (string-join (map emit-expr exprs) ", "))

(define (emit-expr expr)
  ;(display "\n\nemit-expr:\n")
  ;(write expr)
  ;(newline)
  (cond
    ((pair? expr)
     (if (eqv? (car expr) 'quote)
         (emit-expr (cdr expr))
         (apply apply-go expr)))
    ((char? expr) (emit-char expr))
    ((boolean? expr) (if expr "true" "false"))
    ((number? expr) (number->string expr))
    ((string? expr) (emit-string expr))
    ((symbol? expr) (emit-symbol expr))
    ((vector? expr) (error "Vectors are no longer used for literals, please use go:new: or go:make: instead."))
    ;(emit-literal (vector->list expr)))
    ((null? expr) "null()")
    (else (error "emit unrecognized type"))))

(define (map-go sy xs)
  (define (fn x) (apply apply-go sy x))
  (map fn xs))
  
(define (join-go . rest)
  (cond
   ((null? rest) "")
   ((pair? rest)
    (let* ((expr (car rest))
           (exprs (cdr rest)))
      (string-append
       (cond
        ((string? expr) expr)
        ;((and (list? expr)
        ;      (eqv? (car expr) 'map))
        ; (let ((sy (cadr expr))
        ;       (xs (caddr expr)))
        ;   (string-join (map-go sy xs) " ")))
        (else (emit-expr expr)))
       " "
       (apply join-go exprs))))
   (else (error "unexpected"))))

(define (apply-go . expr)
  (define (do-match)
    (let ((m (apply *rules* expr)))
      (cond ((list? m) (apply join-go m))
            ((string? m) m)
            (else #f)))) ;(error "apply-go unexpected" expr m)))))
  (define (yes-match key . args) #f)
  ;(define (no-match key . args) #f)
  ;(define (yes-match key . args) (error "yes-match" args))
  (define (no-match key . args) (error "no-match" args))
  (let ((t (catch 'misc-error 
                  (lambda () 
                    (catch 'match-error do-match no-match))
                  yes-match)))
    (if t t (apply apply-go 'go:call expr))))

(define (*rules* . expr)
  (match expr

    ;; binary operators
    (('go:!= . vals)      (apply emit-binary expr))
    (('go:% . vals)       (apply emit-binary expr))
    (('go:%= vars . vals) (apply emit-assign expr))
    (('go:* . vals)       (apply emit-binary expr))
    (('go:*= vars . vals) (apply emit-assign expr))
    (('go:+ . vals)       (apply emit-binary expr))
    (('go:+= vars . vals) (apply emit-assign expr))
    (('go:- . vals)       (apply emit-binary expr))
    (('go:-= vars . vals) (apply emit-assign expr))
    (('go:/ . vals)       (apply emit-binary expr))
    (('go:/= vars . vals) (apply emit-assign expr))
    (('go::= vars . vals) (apply emit-assign expr))
    (('go:< . vals)       (apply emit-binary expr))
    (('go:<< . vals)      (apply emit-binary expr))
    (('go:<<= vars . vals)(apply emit-assign expr))
    (('go:<= . vals)      (apply emit-binary expr))
    (('go:= vars . vals)  (apply emit-assign expr))
    (('go:== . vals)      (apply emit-binary expr))
    (('go:> . vals)       (apply emit-binary expr))
    (('go:>= . vals)      (apply emit-binary expr))
    (('go:>> . vals)      (apply emit-binary expr))
    (('go:>>= vars . vals)(apply emit-assign expr))
    (('go:and . vals)     (apply emit-binary expr))
    (('go:dot . vals)     (apply emit-binary expr))
    (('go:or . vals)      (apply emit-binary expr))

    ;; bitwise operators
    (('go:bitwise-and . vals)       (apply emit-binary expr))
    (('go:bitwise-and= vars . vals) (apply emit-assign expr))
    (('go:bitwise-but . vals)       (apply emit-binary expr))
    (('go:bitwise-but= vars . vals) (apply emit-assign expr))
    (('go:bitwise-or . vals)        (apply emit-binary expr))
    (('go:bitwise-or= vars . vals)  (apply emit-assign expr))
    (('go:bitwise-xor . vals)       (apply emit-binary expr))
    (('go:bitwise-xor= vars . vals) (apply emit-assign expr))

    ;; other operators
    (('go:++ expr) `(,expr "++"))
    (('go:-- expr) `(,expr "--"))
    (('go:: key value) `(,(emit-expr key) ": " ,(emit-expr value)))
    (('go:<- chan) `("<-" ,chan))
    (('go:<-! chan expr) `(,chan "<-" ,expr))
    (('go:adr expr) `("&" ,expr))
    (('go:as . body) (apply emit-as body))
    (('go:not expr) `("!" ,expr))
    (('go:ptr expr) `("*" ,expr))
    (('go:label id . stmts) `(,id ":" ,(emit-stmts stmts)))
    (('go:block . body) (emit-block body))

    ;; keywords
    (('go:call fn . args)
     `(,fn "(" ,(emit-exprs args) ")"))
    (('go:apply fn . args)
     `(,fn "(" ,(emit-exprs args) "...)"))
    (('go:array length type)
     `("[" ,length "]" ,type))
    (('go:array... type)
     `("[...]" ,type))
    (('go:values . types)
     `("(" ,(emit-params types) ")"))
    (('go:ellipsis type)
     `("..." ,type))
    (('go:slice type)
     `("[]" ,type))
    (('go:struct . fields)
     `("struct" ,(emit-type-block fields)))
    (('go:map: key-type type)
     `("map" "[" ,key-type "]" ,type))
    (('go:chan type) `("chan" ,type))
    (('go:chan<- type) `("<-chan" ,type))
    (('go:chan<-! type) `("chan<-" ,type))
    (('go:interface . methods)
     `("interface" ,(emit-interface-block methods)))

    (('go:new: . args)
     `("&" ,(emit-literal args)))
    (('go:make: . args)
     (emit-literal args))

    (('go:break . rest) (emit-branch 'break rest))
    (('go:continue . rest) (emit-branch 'continue rest))
    (('go:fallthrough . rest) (emit-branch 'fallthrough rest))
    (('go:goto . rest) (emit-branch 'goto rest))

    (('go:for a b c . body)
     `("for" ,a ";" ,b ";" ,c ,(emit-block body)))
    (('go:while c . body)
     `("for" ,c ,(emit-block body)))
    (('go:range a . body)
     `("for" ,(emit-range a) ,(emit-block body)))

    (('go:branch-stmt kw . label)
     `(kw ,@label))
    (('go:package name . decls)
     `("package" ,name "\n" ,(emit-decls decls)))
    (('go:return . expr)
     `("return" ,(emit-exprs expr)))
    (('go:defer expr)
     `("defer" ,expr))

    ;; The "if/else" keywords
    (('go:when expr . body)
     `("if" ,expr ,(emit-else-block body)))
    (('go:when* stmt expr . body)
     `("if" ,stmt ";" ,expr ,(emit-else-block body)))
    (('go:unless expr . body)
     `("if !" ,expr ,(emit-else-block body)))
    (('go:unless* stmt expr . body)
     `("if" ,stmt "; !" ,expr ,(emit-else-block body)))


    ;; The "switch/select" keywords
    (('go:case! expr . body)
     `("switch" ,expr ,(emit-cases body)))
    (('go:case!* stmt expr . body)
     `("switch" ,stmt ";" ,expr ,(emit-cases body)))
    (('go:comm! . body)
     `("select" ,(emit-conds body)))
    (('go:cond! . body)
     `("switch" ,(emit-conds body)))
    (('go:cond!* stmt . body)
     `("switch" stmt ";" ,(emit-conds body)))
    (('go:type! expr . body)
     `("switch" ,expr ,(emit-cases body)))
    (('go:type!* stmt expr . body)
     `("switch" ,stmt ";" ,expr ,(emit-cases body)))

    (('go:index expr j . ks)
     (let ((offset (lambda (k) (if (not k) "" (emit-expr k)))))
       (if (pair? ks)
           `(,expr "[" ,(offset j) ":" ,(offset (car ks)) "]")
           `(,expr "[" ,(offset j) "]"))))

    (('go:import . specs)
     (emit-parens "import" emit-imports specs))
    (('go:type . specs)
     (emit-type-parens "type" emit-types specs))
    (('go:const . specs)
     (emit-parens "const" emit-values specs))
    (('go:var . specs)
     (emit-parens "var" emit-values specs))

    (('go:internal:define-func name sig ret . body)
     (if (*top-context*)
         `("func" ,name ,(emit-sig sig ret) ,(emit-block body)) ; FuncDecl
         `("var" ,name " func" ,(emit-sig sig ret) "\n" 
               ,name " = func" ,(emit-sig sig ret) ,(emit-block body)))) ; FuncStmt

    (('go:internal:define-method-func rec name sig ret . body)
     `("func (" ,(emit-field rec) ")" ,name 
                ,(emit-sig sig ret) 
                ,(emit-block body)))

    (('go:internal:lambda-func sig ret . body)
     (if (*type-context*)
         `("func" ,(emit-sig sig ret)) ; FuncType
         `("func" ,(emit-sig sig ret) 
                  ,(emit-block body)))) ; FuncExpr

    ;; The glorious "func" type-switch!
    (('go:func . rest)
     (cond
      ((vector? (car rest)) `((go:internal:define-method-func ,@rest)))
      ((symbol? (car rest)) `((go:internal:define-func ,@rest)))
      ((list? (car rest)) `((go:internal:lambda-func ,@rest)))
      (else (error "func expected symbol, vector, or list"))))

  );match
);define
