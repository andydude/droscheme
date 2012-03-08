(define-library (scheme base)

  (export
    *
    +
    -
    /
    abs
    append
    apply
    assoc
    assq
    assv
    binary-port?
    boolean?
    bytevector-copy
    bytevector-copy!
    bytevector-copy-partial
    bytevector-copy-partial!
    bytevector-length
    bytevector-u8-ref
    bytevector-u8-set!
    bytevector?
    call-with-current-continuation
    call-with-port
    call-with-values
    call/cc
    car
    cdr
    ceiling
    char->integer
    char-ready?
    char<=?
    char<?
    char=?
    char>=?
    char>?
    char?
    close-input-port
    close-output-port
    close-port
    complex?
    cons
    current-error-port
    current-input-port
    current-output-port
    denominator
    dynamic-wind
    eof-object?
    eq?
    equal?
    eqv?
    error
    error-object-irritants
    error-object-message
    error-object?
    even?
    exact->inexact
    exact-integer-sqrt
    exact-integer?
    exact?
    expt
    floor
    flush-output-port
    for-each
    gcd
    get-output-bytevector
    get-output-string
    inexact->exact
    inexact?
    input-port?
    integer->char
    integer?
    lcm
    length
    list
    list->string
    list->vector
    list-copy
    list-ref
    list-set!
    list-tail
    list?
    make-bytevector
    make-list
    make-parameter
    make-string
    make-vector
    map
    max
    member
    memq
    memv
    min
    modulo
    negative?
    newline
    not
    null?
    number->string
    number?
    numerator
    odd?
    open-input-bytevector
    open-input-string
    open-output-bytevector
    open-output-string
    output-port?
    pair?
    peek-char
    peek-u8
    port-open?
    port?
    positive?
    procedure?
    quotient
    raise
    raise-continuable
    rational?
    rationalize
    read-bytevector
    read-bytevector!
    read-char
    read-line
    read-u8
    real?
    remainder
    reverse
    round
    set-car!
    set-cdr!
    string
    string->list
    string->number
    string->symbol
    string->utf8
    string->vector
    string-append
    string-copy
    string-fill!
    string-for-each
    string-length
    string-map
    string-ref
    string-set!
    string<=?
    string<?
    string=?
    string>=?
    string>?
    string?
    substring
    symbol->string
    symbol?
    textual-port?
    truncate
    u8-ready?
    utf8->string
    values
    vector
    vector->list
    vector->string
    vector-copy
    vector-fill!
    vector-for-each
    vector-length
    vector-map
    vector-ref
    vector-set
    vector?
    with-exception-handler
    write-bytevector
    write-char
    write-partial-bytevector
    write-u8
    zero?)

  (import
   (rename
    (scheme division)
    (truncate-quotient quotient)
    (truncate-remainder remainder)
    (floor-remainder modulo)))

; Syntax Forms
(define-native-syntax define "ds_Scheme_define")
(define-native-syntax define-syntax "ds_Scheme_define_syntax")
(define-native-syntax syntax-rules "ds_Scheme_syntax_rules")
(define-native-syntax identifier-syntax "ds_Scheme_identifier_syntax")
(define-native-syntax if "ds_Scheme_if")
(define-native-syntax lambda "ds_Scheme_lambda")
(define-native-syntax unquote "ds_Scheme_unquote")
(define-native-syntax unquote-splicing "ds_Scheme_unquote_splicing")

; Procedures

(define-native *) ;ZH
(define-native +) ;ZI
(define-native -) ;ZK
(define-native /) ;ZM

(define (= . args) (apply ds:equal args))
(define (< . args) (for-all (lambda (x) (= -1 x)) (apply ds:compare args)))
(define (<= . args) (for-all (lambda (x) (not (= 1 x))) (apply ds:compare args)))
(define (> . args) (for-all (lambda (x) (= 1 x)) (apply ds:compare args)))
(define (>= . args) (for-all (lambda (x) (not (= -1 x))) (apply ds:compare args)))
(define (abs x) (if (negative? x) (- x) x))
(define-c acos "ds_Number_acos")
(define-native angle)
(define-native append)
(define-native apply)
(define-native assert)
(define-native assertion-violation)
(define-c atan "ds_Number_atan")
(define-native begin)
(define-native boolean?)
(define call/cc call-with-current-continuation)
(define-native call-with-current-continuation)
(define-native call-with-values)
(define-c car "ds_List1_first")
(define-c cdr "ds_List1_rest")

(define caar (ds:compose2 car car))
(define cadr (ds:compose2 car cdr))
(define cdar (ds:compose2 cdr car))
(define cddr (ds:compose2 cdr cdr))

(define caaar (ds:compose3 car car car))
(define caadr (ds:compose3 car car cdr))
(define cadar (ds:compose3 car cdr car))
(define caddr (ds:compose3 car cdr cdr))
(define cdaar (ds:compose3 cdr car car))
(define cdadr (ds:compose3 cdr car cdr))
(define cddar (ds:compose3 cdr cdr car))
(define cdddr (ds:compose3 cdr cdr cdr))

(define caaaar (ds:compose4 car car car car))
(define caaadr (ds:compose4 car car car cdr))
(define caadar (ds:compose4 car car cdr car))
(define caaddr (ds:compose4 car car cdr cdr))
(define cadaar (ds:compose4 car cdr car car))
(define cadadr (ds:compose4 car cdr car cdr))
(define caddar (ds:compose4 car cdr cdr car))
(define cadddr (ds:compose4 car cdr cdr cdr))
(define cdaaar (ds:compose4 cdr car car car))
(define cdaadr (ds:compose4 cdr car car cdr))
(define cdadar (ds:compose4 cdr car cdr car))
(define cdaddr (ds:compose4 cdr car cdr cdr))
(define cddaar (ds:compose4 cdr cdr car car))
(define cddadr (ds:compose4 cdr cdr car cdr))
(define cdddar (ds:compose4 cdr cdr cdr car))
(define cddddr (ds:compose4 cdr cdr cdr cdr))

(define-native ceiling)
(define-native char->integer)
(define (ds:real-compare a b)
  (cond [(> a b) 1]
        [(= a b) 0]
        [(< a b) -1]))
(define (ds:char-compare a b)
  (ds:real-compare (char->integer a) (char->integer b)))

(define (char<=? . args) (fold ds:char-compare args))
(define (char<?  . args) (fold ds:char-compare args))
(define (char=?  . args) (fold ds:char-compare args))
(define (char>=? . args) (fold ds:char-compare args))
(define (char>?  . args) (fold ds:char-compare args))


(define-native char?)
(define-native complex?)
(define-native cons)
(define-native denominator)
(define-c div "ds_Number_div_EUC_")
(define-c div-and-mod "ds_Number_divmod_EUC_")
(define-c div0 "ds_Number_div_RNN_")
(define-c div0-and-mod0 "ds_Number_divmod_RNN_")
(define-native dynamic-wind)
(define-native eq?)
(define-native equal?)
(define-native eqv?)
(define-native error)
(define-native even?)
(define-native exact?)
(define-native exp)
(define-native expt)
(define-native finite?)
(define-native floor)
(define-native for-each)
(define-native gcd)
(define-native imag-part)
(define-native inexact)
(define-native inexact?)
(define-native infinite?)
(define-native integer->char)
(define-native integer-valued?)
(define-native integer?)
(define-native lcm)
(define-native length)
(define-native list->string)
(define-native list->vector)
(define-native list-ref)
(define-native list-tail)
(define-native list?)
(define-native log)
(define-native magnitude)
(define-native make-polar)
(define-native make-rectangular)
(define-native map)
(define-native max)
(define-native min)
(define-c mod "ds_Number_mod_EUC_")
(define-c mod0 "ds_Number_mod_RNN_")
(define-native nan?)
(define (negative? x) (< x 0))
(define-native not)
(define-native null?)
(define-native number?)
(define-native number->string)
(define-native numerator)
(define (odd? x) (integer? (/ (+ x 1) 2)))
(define-native pair?)
(define (positive? x) (> x 0))
(define-native procedure?)
(define-native rational-valued?)
(define-native rational?)
(define-native rationalize)
(define-native real-part)
(define-native real?)
(define-native reverse)
(define-native round)
(define-native set!)
(define-native sqrt)
(define-native string->list)
(define-native string->number)
(define-native string->symbol)
(define-native string-append)
(define-native string-copy)
(define-native string-for-each)
(define-native string-ref)
(define-native string=?)
(define-native string?)
(define-native symbol->string)
(define-native symbol?)
(define-c truncate "ds_Number_round_???_")
(define-native vector)
(define-native vector->list)
(define-native vector-for-each)
(define-native vector-length)
(define-native vector-map)
(define-native vector-ref)
(define-native vector-set!)
(define-native vector?)
(define-native zero?)


(define-native-sequence bytevector 		"ds_Binary")
(define-native-sequence complex 		"ds_Complex")
(define-native-sequence complex-polar 	"ds_ComplexPolar")
(define-native-sequence list 			"ds_List1")
(define-native-sequence rational 		"ds_Rational")
(define-native-sequence string 			"ds_String")
(define-native-sequence symbol 			"ds_Symbol")
(define-native-sequence vector 			"ds_Array1D")


);library