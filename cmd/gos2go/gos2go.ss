;;/usr/bin/env racket -f
;;/usr/bin/env guile -s
;;/usr/bin/env ds -f
;; -*- mode: scheme -*- !#

(require srfi/13)

;; the first 3 functions were added for guile compatibility

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

(define (char-mangle c)
  (case c
    ((#\!) "ZA")
    ;((#\ ) "ZB")
    ((#\#) "ZC")
    ((#\$) "ZD")
    ((#\%) "ZE")
    ((#\&) "ZF")
    ;((#\ ) "ZG")
    ((#\*) "ZH")
    ((#\+) "ZI")
    ((#\,) "ZJ")
    ((#\-) "ZK")
    ((#\.) "ZL")
    ((#\/) "ZM")
    ((#\:) "ZN")
    ((#\;) "ZO")
    ((#\<) "ZP")
    ((#\=) "ZQ")
    ((#\>) "ZR")
    ((#\?) "ZS")
    ((#\@) "ZT")
    ;((#\\) "ZU")
    ((#\^) "ZV")
    ((#\`) "ZW")
    ((#\|) "ZX")
    ((#\~) "ZY")
    ((#\Z) "ZZ")
    (else (string c))))

(define (string->mangle str)
  (string-join (map char-mangle (string->list str)) ""))

(define (symbol->mangle sym)
  (string->mangle (symbol->string sym)))

(define (symbol-intern sym)
  (string->symbol (string-append "_" (symbol->string sym))))

;; not in racket
(define (string-null? str)
  (eqv? str ""))

;; not in racket
(define (string-split str . rest)
  (define (split-by-whitespace str maxsplit)
    (define (skip-ws i yet-to-split-count)
      (cond
        ((>= i (string-length str)) '())
        ((char-whitespace? (string-ref str i))
          (skip-ws (add1 i) yet-to-split-count))
        (else (scan-beg-word (add1 i) i yet-to-split-count))))
    (define (scan-beg-word i from yet-to-split-count)
      (cond
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word i from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((char-whitespace? (string-ref str i))
          (cons (substring str from i) 
            (skip-ws (add1 i) (- yet-to-split-count 1))))
        (else (scan-word (add1 i) from yet-to-split-count))))
    (skip-ws 0 (- maxsplit 1)))

                ; maxsplit is a positive number
                ; str is not empty
  (define (split-by-charset str delimeters maxsplit)
    (define (scan-beg-word from yet-to-split-count)
      (cond
        ((>= from (string-length str)) '(""))
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word from from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((memq (string-ref str i) delimeters)
          (cons (substring str from i) 
            (scan-beg-word (add1 i) (- yet-to-split-count 1))))
        (else (scan-word (add1 i) from yet-to-split-count))))
    (scan-beg-word 0 (- maxsplit 1)))

                        ; resolver of overloading...
                        ; if omitted, maxsplit defaults to
                        ; (add1 (string-length str))
  (if (string-null? str) '()
    (if (null? rest) 
      (split-by-whitespace str (add1 (string-length str)))
      (let ((charset (car rest))
          (maxsplit
            (if (pair? (cdr rest)) (cadr rest) (add1 (string-length str)))))
        (cond
          ((not (positive? maxsplit)) '())
          ((null? charset) (split-by-whitespace str maxsplit))
          (else (split-by-charset str charset maxsplit))))))
)

;(define (string-split str chr)
;  (list str))
;
;(define (string-join l sep)
;  (if (null? l) ""
;      (fold-right (lambda (a b) (string-append a sep b)) (last l) (most l))))

;(define (string-escape-2 str c s)
;  (string-join (string-split str '(c)) s))

(define (string-escape-2 str c s)
  (regexp-replace c str s))

(define (string-escape a)
  (let* ((b (string-escape-2 a "\\\\" "\\\\\\\\"))
         (c (string-escape-2 b "\\\"" "\\\\\\\""))
         (d (string-escape-2 c "\n" "\\\\n")))
    d))

;; primary definitions

(define (emit-++ x)
  (string-append (emit x) "++"))

(define (emit--- x)
  (string-append (emit x) "--"))

(define (emit-+ . rest)
  (string-join (map emit rest) " + "))

(define (emit-* . rest)
  (string-append "(" (string-join (map emit rest) " * ") ")"))

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

(define (emit-<- expr) expr)
(define (emit-<-! expr) expr)
(define (emit-<-chan t) (string-append "<-chan " (emit t)))
(define (emit-array n t) (string-append "[" (emit-number n) "]" (emit t)))

(define (emit-adr expr)
  (string-append "&" (emit expr)))

(define (emit-body proc rest)
  (string-join (map proc rest) "\n\t"))

;(define (emit-toa ty ob)
;  (string-append (emit ty) "(" (emit ob) ")"))
;
;(define (emit-isa ob ty)
;  (string-append (emit ob) ".(" (emit ty) ")"))

(define (emit-if2 opt condition . rest)
  (string-append "if "
     (string-append (emit opt) "; ")
     (emit condition) " {\n"
     (string-join (map emit rest) "\n\t")
     "\n}\n"))

(define (emit-if1 condition . rest)
  (string-append "if "
     (emit condition) " {\n"
     (string-join (map emit rest) "\n\t")
     "\n}\n"))

(define (emit-dot . rest)
  (string-join (map emit rest) "."))

(define (emit-as a b . rest)
  (if (null? rest)
      (string-append (emit a) ".(" (emit b) ")")
      (apply emit-dot (list 'as a b) rest)))

(define (emit-else . rest)
  (string-append "\n} else {\n\t"
     (string-join (map emit rest) "\n\t")))

(define (emit-function-name proc)
  (string-append
    (if (and (symbol? proc) 
             (char-upper-case? 
              (car (string->list 
                    (symbol->string proc)))))
        ""
        "_")
    (emit proc)))

(define (emit-call proc . rest)
  (string-append
    (emit-function-name proc)
    "("
    (string-join (map emit rest) ", ")
    ")"))

(define (emit-gocall proc . rest)
  (string-append
    (emit proc)
    "("
    (string-join (map emit rest) ", ")
    ")"))

(define (emit-not a)
  (string-append "(! " (emit a) ")"))
(define (emit-list-and ls . rest)
  (string-append "(" (string-join ls " && ") ")"))
(define (emit-list-or ls . rest)
  (string-append "(" (string-join ls " || ") ")"))
(define (emit-and . rest)
  (string-append "(" (string-join (map emit rest) " && ") ")"))
(define (emit-or . rest)
  (string-append "(" (string-join (map emit rest) " || ") ")"))

(define (emit-bool b) (if b "true" "false"))
(define (emit-char c) (string-append "'" (list->string (list c)) "'"))

(define (emit-chan t) (string-append "chan " (emit t)))
(define (emit-chan<- t) (string-append "chan<- " (emit t)))

(define (emit-comment text) (string-append "// " text "\n"))

(define (emit-preellipsis name)
  (string-append "..." (emit name)))

(define (emit-postellipsis name)
  (string-append (emit name) "..."))

(define (emit-define-go fsig . rest)
  (string-append
         "func "
         (emit-method-signature fsig)
         (emit-braces emit rest)))

(define (argument-prelist ls)
  (if (null? ls) '()
      (if (pair? ls)
          (cons `(,(car ls) Any) (argument-prelist (cdr ls)))
          (list `(,ls (preellipsis Any))))))

(define (argument-list ls)
  (let ((pre (argument-prelist ls)))
    (if (and (pair? (last pre))
             (pair? (cdr (last pre)))
             (pair? (cadr (last pre))))
        (if (null? (most pre))
            (list (last pre))
            (list (append (map car (most pre)) '(Any)) (last pre)))
        (if (null? pre) '()
            (list (append (map car pre) '(Any)))))))

(define (emit-define fsig . rest)
  (append-defines (car fsig))
  (apply emit-define-go 
         (append (list (car fsig)) (argument-list (cdr fsig)) '(Any))
         (append (most rest)
                 (list (cons 'return (list (last rest)))))))

(define (emit-for1 c . rest)
  (string-append
    "for "
    (emit c)
    (emit-braces emit rest)))

(define (emit-for3 c b a . rest)
  (string-append
    "for "
    (emit c)
    "; "
    (emit b)
    "; "
    (emit a)
    (emit-braces emit rest)))

(define (emit-func . rest)
  (emit-signature (cons 'func rest)))

(define (emit-inline . rest)
  (apply string-append rest))

(define (emit-import . rest)
  (emit-parens "import" emit-string rest))

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

(define (emit-package name . rest)
  (apply string-append
         "package " (emit-symbol name) "\n\n"
         (append (map emit rest)
                 (list (emit-defines)))))

(define (emit-ptr t)
  (string-append "*" (emit t)))

(define (emit-index t k)
  (string-append (emit t) "[" (emit k) "]"))

(define (emit-signature sig)
  (let ((fn (car sig))
        (ar (cdr sig)))
    (if (null? ar) "()"
        (let ((ag (most (cdr sig)))
              (rs (last (cdr sig))))
          (string-append
           ;(emit-symbol fn)
           (emit-function-name fn)
           (emit-signature-args ag)
           (if (pair? rs)
               (let ((r (car rs)))
                 (if (eqv? r 'void)
                     ""
                     (if (eqv? r 'values)
                         (emit-signature-args (cdr rs))
                         (emit-signature-args (list rs)))))
               (emit rs)))))))

(define (emit-signature-arg arg)
  (let ((vs (most arg))
        (ts (last arg)))
    (string-append (string-join (map emit vs) ", ") " " (emit ts))))

(define (emit-signature-args sig)
  (string-append "(" (string-join (map emit-signature-arg sig) ", ") ")"))


(define (emit-slice t) (string-append "[" "]" (emit t)))

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
  (if (symbol? id)
      (symbol->mangle id)
      (begin
        (error id))))

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

(define (emit-:: lhs . rhs)
  (emit-signature-arg lhs))

(define (emit-assign typed op lhs rhs)
  (if (pair? lhs)
      (string-append
       ((if typed
            emit-signature-arg
            emit-valuess) lhs) " " op " "
       (emit-valuess rhs))
      (string-append
       (emit lhs) " " op " " (emit (car rhs)))))

(define (emit-const . rest)
  (emit-parens "const" emit-var-spec rest))

(define (emit-const-spec ls)
  (if (eqv? '= (car ls))
      (apply emit-= (cdr ls))
      (emit-signature-arg ls)))

(define (emit-return . rest)
  (if (null? rest) "return"
      (string-append "return " (emit-valuess rest))))

(define (emit-var . rest)
  (emit-parens "var" emit-var-spec rest))

(define (emit-var-spec ls)
  (if (eqv? '= (car ls))
      (apply emit-= (cdr ls))
      (emit-signature-arg ls)))

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

(define (emit-values . rest)
  (string-join (map emit rest) ", "))

(define (emit-valuess arg)
  (string-join (map emit arg) ", "))

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

(define (emit expr)
  (if (pair? expr)
      (if (assoc (car expr) syntax-table)
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
       (else (error "What?!?")))))

(define syntax-table
  (list
;    (cons '<-          emit-<-         )
;    (cons '<-!         emit-<-!        )
;    (cons '<-chan      emit-<-chan     )
    (cons '+           emit-+          )
    (cons '++          emit-++         )
    (cons '--          emit---         )
    (cons '*           emit-*          )
    (cons '<           emit-<          )
    (cons '<=          emit-<=          )
    (cons '>           emit->           )
    (cons '>=          emit->=          )
    (cons '==          emit-==          )
    (cons '=           emit-=          )
    (cons '=:          emit-=:         )
    (cons ':=          emit-:=         )
    (cons ':=:         emit-:=:        )
    (cons '::          emit-::         )
    (cons 'adr         emit-adr        )
    (cons 'bool-and    emit-and        )
    (cons 'bool-or     emit-or         )
    (cons 'array       emit-array      )
    (cons 'break       emit-break      )
    (cons 'call        emit-call       )
    (cons 'call-go     emit-gocall     )
    (cons 'preellipsis    emit-preellipsis     )
    (cons 'postellipsis    emit-postellipsis     )
;    (cons 'case        emit-case       )
;    (cons 'chan        emit-chan       )
;    (cons 'chan<-      emit-chan<-     )
    (cons 'const       emit-const      )
    (cons 'comment     emit-comment    )
    (cons 'continue    emit-continue   )
;    (cons 'default     emit-default    )
    (cons 'defer       emit-defer      )
    (cons 'define      emit-define     )
    (cons 'define-go   emit-define-go  )
    (cons 'dot         emit-dot        )
    (cons 'as          emit-as         )
    (cons 'else        emit-else       )
    (cons 'fallthrough emit-fallthrough)
    (cons 'for         emit-for3       )
    (cons 'for1        emit-for1       )
    (cons 'func        emit-func       )
    (cons 'fn          emit-function-name)
;    (cons 'go          emit-go         )
    (cons 'goto        emit-goto       )
    (cons 'if1         emit-if1        )
    (cons 'if2         emit-if2        )
;    (cons 'isa         emit-isa        )
;    (cons 'to          emit-toa        )
    (cons 'inline      emit-inline     )
    (cons 'import      emit-import     )
    (cons 'interface   emit-interface  )
;    (cons 'map         emit-map        )
    (cons 'bool-not    emit-not        )
    (cons 'package     emit-package    )
    (cons 'ptr         emit-ptr        )
    (cons 'index       emit-index      )
;    (cons 'range       emit-range      )
    (cons 'return      emit-return     )
;    (cons 'select      emit-select     )
    (cons 'slice       emit-slice      )
    (cons 'struct      emit-struct     )
;    (cons 'switch      emit-switch     )
    (cons 'type        emit-type       )
;    (cons 'values      emit-values     )
;    (cons 'void        emit-void       )
    (cons 'var         emit-var        )))

(define (eval-symbol y)
  (if (symbol? y)
      (cdr (assoc y syntax-table))
      (error "expected symbol")))

;; droscheme
;(define (call-with-input-file fn proc)
;  (call-with-port (open-input-file fn) read))

;; globals

(define (emit-defines)
  (define (emit-defines-registration def)
    `(dot env (call-go registerGos (fn ,def))))
  (define (basename st)
    (if (not (string-index-right st #\/))
        (substring st 0 (- (string-length st) 4))
        (let ((start (string-index-right st #\/))
              (end (string-length st)))
          (substring st (+ start 1) (- end 4)))))
  (let ((name (basename *input-filename*)))
    (apply emit-define-go
           (list (string->symbol (string-append "Export_" name))
                 '(env (ptr Env)) '(void))
           (map emit-defines-registration (reverse (*defines*))))))

(define (append-defines name)
  (*defines* (cons name (*defines*))))

(define *defines* (make-parameter '()))
(define *emit-function* (make-parameter emit))
(define *input-filename* "/dev/null")

(define (compile fn)
  (begin
    (display "// generated by gos2go from ")
    (display fn)
    (newline)
    (display (emit (call-with-input-file fn read)))
    (newline)))

;; main

(define (main-guile)
  (begin
    (set! *input-filename* (list-ref (command-line) 1))
    (compile *input-filename*)))

(define (main-racket)
  (begin
    (set! *input-filename* (vector-ref (current-command-line-arguments) 0))
    (compile *input-filename*)))

;(main-guile)
(main-racket)
