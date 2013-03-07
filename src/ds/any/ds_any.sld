(define-library
 (ds any)
 (import
  (ds any char)
  (ds any error)
  "reflect")
 (export
  any
  any1
  append
  append!
  apply
  apply-list
  apply-vector
  boolean
  boolean-and
  boolean-or
  boolean=?
  boolean?
  bytevector
  bytevector->u8-list
  bytevector->u8-vector
  bytevector?
  call
  car
  cars
  cars+
  cdr
  cdrs
  char->integer
  char=?
  char?
  complex128?
  complex64?
  cons
  empty?
  eof-object
  eof-object?
  eq?
  equal?
  eqv?
  every
  every1
  exact-if?
  exact=?
  exact?
  float32?
  float64?
  fold
  fold-right
  imm-list?
  imm-null
  imm-null?
  imm-pair?
  imm-star-length
  imm-star?
  imm-string->string
  imm-string=?
  imm-string?
  inexact-if?
  inexact=?
  inexact?
  integer->char
  kind->string
  kind-of
  last-pair
  length
  list
  list*
  list*->vector
  list+
  list->string
  list->vector
  list?
  list?
  m-null?
  m-pair?
  m-star?
  m-string?
  make-bytevector
  make-environment
  make-imm-list
  make-list
  make-primitive
  make-promisedone?
  make-string
  make-vector
  map
  map1
  not
  null
  null?
  object-equal?
  object-hash
  pair?
  pointer-of
  pointer=?
  primitive-apply-vector
  procedure?
  s16?
  s32?
  s64?
  s8?
  string
  string->imm-string
  string->list
  string->symbol
  string->vector
  string-append
  string-hash
  string?
  symbol->string
  symbol=?
  symbol?
  type-check
  type-error
  type=?
  u16?
  u32->bytevector
  u32?
  u64?
  u8-list->bytevector
  u8-vector->bytevector
  u8?
  u8?
  vector
  vector
  vector->list
  vector->list*
  vector->string
  vector-length
  vector-map
  vector?
  void
  void?
  ->go-string
  ->imm-string
  ->scheme-string
  ->string)
 (begin

   (define (append a . rest)
     (if (null? rest) a
         (fold-right cons (apply append rest) a)))

   (define (apply proc . args)
     (apply-list proc (apply-list list* args)))

   (define (apply-list proc args)
     (apply-vector proc (list->vector args)))

   (define (apply-vector proc args)
     (go:when* (go::= (applier ok) (go:as proc Applier)) ok
               (go:return (applier.Apply (go:as args go:internal:vector))))
     (primitive-apply-vector proc args))

   (define (primitive-apply-vector proc args)
     (values->object ((go:dot (object->value proc) Call) 
                              (vector->values args))))

   (define (boolean-and . rest)
     ;; fast
     (go:case! (go:len rest)
      ((0) (go:return #t))
      ((1) (go:return (go:index rest 0))))
     ;; iterate
     (go:for (go::= i 0) (go:< i (go:len rest)) (go:++ i)
             (go::= a (go:index rest i))
             (go::= (b ok) (go:as a go:bool))
             (go:when (go:and ok (go:== b #f))
                      (go:return #f)))
     #t)

   (define (boolean-or . rest)
     ;; fast
     (go:case! (go:len rest)
      ((0) (go:return #f))
      ((1) (go:return (go:index rest 0))))
     ;; iterate
     (go:for (go::= i 0) (go:< i (go:len rest)) (go:++ i)
             (go::= a (go:index rest i))
             (go::= (b ok) (go:as a go:bool))
             (go:when (go:or (go:not ok) (go:!= b #f))
                      (go:return a)))
     #t)

   (define (boolean=? a b)
     (type-check boolean? a)
     (type-check boolean? b)
     (go:== (go:as a go:bool) 
            (go:as b go:bool)))

   (define (boolean? a)
     (go::= (_ ok) (go:as a go:bool))
     ok)

   (define (boolean a)
     (go:when* (go::= (bl ok) (go:as a go:bool)) ok
            (go:return (go:!= bl #f)))
     #t)
           

;   (define (bytevector . rest)
;     (u8-vector->bytevector rest))

   (define (bytevector->u8-list a)
     (vector->list (bytevector->u8-vector a)))

   (define (bytevector->u8-vector a)
     (go::= bv (go:as a go:internal:binary))
     (go::= vc (go:as (make-vector (go:len bv)) go:internal:vector))
     (go:for (go::= i 0) (go:< i (go:len bv)) (go:++ i)
             (go:= ((go:index vc i))
                   (go:index bv i)))
     vc)
   
   (define (bytevector? a)
     (go::= (_ ok) (go:as a (go:slice go:byte)))
     ok)

   (define (call proc . rest)
     (apply-vector proc rest))

   (define (car a)
     (go:when* (go::= (ls ok) (go:as a Conser)) ok
               (go:return ((go:dot ls Car))))
     (type-error pair? a))

   (define (cdr a)
     (go:when* (go::= (ls ok) (go:as a Conser)) ok
       (go:return ((go:dot ls Cdr))))
     (type-error pair? a))

   (go:func car+cdr (ls) (values #(fst go:any) #(rst go:any))
     (go:return (car ls) (cdr ls)))

   (define (cons a b)
     (go:when* (go::= (ls ok) (go:as b Conser)) ok
       (go:return ((go:dot ls Cons) a)))
     (type-error pair? b))

;   (define (empty? a) #f)

   (define (eof-object)
     (go:rune 65532))
   
   (define (eof-object? a)
     (go:== (go:as a go:rune) 65532))

;   (define (char? a)
;     (go::= (_ ok) (go:as a go:rune))
;     ok)
;
;   (define (eq? a b)
;     (eqv? a b))

   (define (equal? a b)
     ;(go:when (go:as (eqv? a b) go:bool)
     ;  (go:return #t))
     (go:when* (go::= (eq ok) (go:as a Equaler)) ok
       (go:return ((go:dot eq Equal) b)))
     (object-equal? a b))

;   (define (eqv? a b)
;     (unless (type=? a b) (go:return #f))
;     (when (symbol? a)    (go:return (symbol=? a b)))
;     (when (boolean? a)   (go:return (boolean=? a b)))
;     (when (inexact? a)   (go:return (inexact=? a b)))
;     (when (exact? a)     (go:return (exact=? a b)))
;     (when (char? a)      (go:return (char=? a b)))
;     (when (null? a)      (go:return (null? b)))
;     (when (empty? a)     (go:return (empty? b)))
;     (pointer=? a b))

;   ;; TODO
;   (define (exact=? a b)
;     (object-equal? a b))
;
;   (define (exact? a)
;     (or (s64? a)
;         (s32? a)
;         (s16? a)
;         (s8? a)
;         (u64? a)
;         (u32? a)
;         (u16? a)
;         (u8? a)))
;
;   ;(define (exact-if? a)
;   ;  (go:when* (go::= (num ok) (go:as a Num)) ok
;   ;            (go:dot num (IsExact)))
;   ;  #f)
;   ;
;   ;(define (inexact-if? a)
;   ;  (go:when* (go::= (num ok) (go:as a Num)) ok
;   ;            (go:dot num (IsInexact)))
;   ;  #f)

   (define (append! . rest)
     (apply append rest))

   (define (cdrs lists)
     (map cdr lists))

   (define (cars lists)
     (map car lists))

   (define (cars+ lists last-elt)
     (append! (map car lists) (list last-elt)))

   ;(go:func cars+cdrs (lists) (#(as go:any) #(ds go:any))
   ;  (values (cars lists) (cdrs lists)))
   ;
   ;(go:func cars+cdrs+ (lists cars-final) (#(as go:any) #(ds go:any))
   ;  (values (append (cars lists) (list cars-final)) 
   ;          (cdrs lists)))
   ;
   ;(define (fold kons knil lis1 . lists)
   ;  (if (pair? lists)
   ;      (let lp ((lists (cons lis1 lists)) (ans knil))	; N-ary case
   ;        (go::= (cars+ans cdrs) (cars+cdrs+ lists ans))
   ;        (if (null? cars+ans) ans ; Done.
   ;            (lp cdrs (apply kons cars+ans))))
   ;      (let lp ((lis lis1) (ans knil))			; Fast path
   ;        (if (null? lis) ans
   ;            (lp (cdr lis) (call kons (car lis) ans))))))
   
   
   (define (fold-right kons knil lis1 . lists)
     (if (pair? lists)
         (let recur ((lists (cons lis1 lists)))		; N-ary case
           (let ((cdrs (map cdr lists)))
             (if (null? cdrs) knil
                 (apply kons (cars+ lists (recur cdrs))))))
         (let recur ((lis lis1))				; Fast path
           (if (null? lis) knil
               (let ((head (car lis)))
                 (call kons head (recur (cdr lis))))))))

;   (define (inexact=? a b)
;     (object-equal? a b))
;
;   (define (inexact? a)
;     (or (float64? a)
;         (float32? a)
;         (complex128? a)
;         (complex64? a)))

   (define (imm-list? a)
     (go::= (_ ok) (go:as a Star))
     ok)

   (define (imm-null . o)
     (go:make: Null))

   (define (imm-null? a)
     (go::= (_ ok) (go:as a Null))
     ok)

   (define (imm-pair? a)
     (go::= (_ ok) (go:as a Pair))
     ok)

   (define (imm-star? a)
     (go::= (_ ok) (go:as a Star))
     ok)

   ;; TODO
   (define (imm-star-length a)
     (go::= (_ ok) (go:as a Star))
     ok)
   
;   (define (imm-string->string a)
;     (go:when* (go::= (s ok) (go:as a go:string)) ok
;               (go:return (go:internal:string s)))
;     (error "expected imm-string"))
;
;   (define (imm-string=? a b)
;     (go:unless
;      (go:as (imm-string? a) go:bool)
;      (type-error "imm-string=?" a))
;     (go:unless
;      (go:as (imm-string? b) go:bool)
;      (type-error "imm-string=?" b))
;     (go:== (go:as a go:string) (go:as b go:string)))
;
;   (define (imm-string? a)
;     (go::= (_ ok) (go:as a go:string))
;     ok)
;
;   (define (integer->char a)
;     (go:rune (go:as a go:int32)))
;
;   (define (kind->string ak)
;     (go:case!
;      (go:as ak go:int)
;      ((KindBool) (go:return "boolean"))
;      ((KindChar) (go:return "char"))
;      ((KindNull) (go:return "null"))
;      ((KindPair) (go:return "pair"))
;      ((KindProc) (go:return "procedure"))
;      ((KindVoid) (go:return "void"))
;      ((KindBinary) (go:return "bytevector"))
;      ((KindString) (go:return "string"))
;      ((KindSymbol) (go:return "symbol"))
;      ((KindValues) (go:return "values"))
;      ((KindVector) (go:return "vector"))
;      ((KindTable) (go:return "hashtable"))
;      ((KindNumber) (go:return "number"))
;      ((KindPort) (go:return "port"))
;      ((KindEnv) (go:return "environment"))
;      ((KindEnvFrame) (go:return "environment-frame"))
;      ((KindSyntax) (go:return "syntax")))
;     (type-error "kind->string" ak)
;     "<unknown-type>")
;
;   (define (kind-of a)
;     (go:when* (go::= (ak ok) (go:as a Kinder)) ok
;               (go:return ((go:dot ak Kind))))
;     (go:type!
;      (go:as a type)
;      ((go:int8) (go:return KindNumber))
;      ((go:int16) (go:return KindNumber))
;      ((go:int32) (go:return KindNumber))
;      ((go:int64) (go:return KindNumber))
;      ((go:uint8) (go:return KindNumber))
;      ((go:uint16) (go:return KindNumber))
;      ((go:uint32) (go:return KindNumber))
;      ((go:uint64) (go:return KindNumber))
;      ((go:float32) (go:return KindNumber))
;      ((go:float64) (go:return KindNumber))
;      ((go:complex64) (go:return KindNumber))
;      ((go:complex128) (go:return KindNumber))
;      ((go:bool) (go:return KindBool))
;      ((go:rune) (go:return KindChar))
;      ((go:string) (go:return KindString))
;      (((go:slice go:rune)) (go:return KindString))
;      (((go:slice go:byte)) (go:return KindBinary))
;      (((go:slice (go:dot reflect Value))) (go:return KindValues))
;      (((go:dot reflect Value)) (go:return KindValues))
;      (((go:slice go:any)) (go:return KindVector))
;      (((go:slice go:internal:frame))
;       (go:return KindEnv))
;      ((go:internal:frame)
;       (go:return KindEnvFrame)))
;     0)
;
;   (define (last-pair a)
;     (go::= cur a)
;     (when (null? cur) (go:return (null)))
;     (go:while (go:as (pair? (cdr cur)) go:bool)
;            (go:= cur (cdr cur)))
;     cur)

   (define (length ls)
     (when (null? ls)
              (go:return 0))
     (go:when* (go::= (_ ok) (go:as ls (go:ptr Pair))) ok
               (go:return (go:+ 1 (go:as (length (cdr ls)) go:int))))
     (error "length expected list"))

   (define (list . o)
     (vector->list o))

   (define (list* . o)
     (go::= (most last)
            (go:index o 0 (go:- (go:len o) 1))
            (go:index o (go:- (go:len o) 1)))
     (vector->list (go:append most last)))

   (define (list+ first . rest)
     (vector->list (go:apply go:append (go:as first go:internal:vector) rest)))

   (define (list->string a)
     (vector->string (list->vector a)))

   (define (list->vector ls)
     ;(go:when (null? ls)
     ;     (go:return (Vector #((go:slice go:any)))))
     (unless (pair? ls)
             (error "list->vector expected list"))
     (go::= (cur vc) (null) (make-vector 8))
     (go:for (go:= cur ls) (go:as (pair? cur) go:bool) (go:= cur (cdr cur))
             (go:= vc (go:call append vc (car cur))))
     (unless (null? cur)
             (error "list->vector expected null"))
     vc)

;   ;(define (list? a)
;   ;  (null? (cdr (last-pair a))))

   (define (list? a)
     (or (m-null? a)
         (m-pair? a)
         (m-star? a)
         (imm-null? a)
         (imm-pair? a)
         (imm-star? a)))

   (define (map1 proc ls)
     (if (null? ls) ()
         (cons (call proc (car ls))
               (map1 proc (cdr ls)))))

   (define (map proc . rest)
     (if (equal? (length rest) 1) (map1 proc (car rest))
         (if (any1 null? rest) ()
             (let ((cars (map1 car rest))
                   (cdrs (map1 cdr rest)))
               (cons (apply proc cars)
                     (apply map proc cdrs))))))

   (define (any1 pred ls)
     (if (null? ls) #f
         (boolean-or (call pred (car ls))
                     (any1 pred (cdr ls)))))

   (define (any pred . rest)
     (if (any1 null? rest) #f
         (boolean-or (apply pred (map1 car rest))
                     (apply any pred (map1 cdr rest)))))

   (define (every1 pred ls)
     (if (null? ls) #t
         (boolean-and (call pred (car ls))
                      (every1 pred (cdr ls)))))

   (define (every pred . rest)
     (if (any1 null? rest) #t
         (boolean-and (apply pred (map1 car rest))
                      (apply every pred (map1 cdr rest)))))

   (define (m-null? a)
     (go::= (_ ok) (go:as a (go:ptr Null)))
     ok)

   (define (m-pair? a)
     (go::= (_ ok) (go:as a (go:ptr Pair)))
     ok)

   (define (m-star? a)
     (go::= (_ ok) (go:as a (go:ptr Star)))
     ok)

   (define (m-string? a)
     (go::= (_ ok) (go:as a (go:slice go:rune)))
     ok)

   (define (make-bytevector k)
     (go:make (go:slice go:byte) (go:as k go:int)))

   (define (make-environment)
     (go:new: Env (go:: it (go:make (go:slice go:internal:frame) 8))))

;   (define (make-imm-list k)
;     (go:make: Star (go:: it (go:make go:internal:vector (go:as k go:int)))))
;
;   (define (make-list k)
;     (vector->m-list (make-vector k)))
;
;   (define (make-primitive-procedure proc name)
;     (go:new: Prim (go:: it proc) (go:: name (go:as name go:string))))

   (define (make-string k)
     (go:make (go:slice go:rune) (go:as k go:int)))

   (define (make-promise done? proc)
     (list (cons done? proc)))

   (define (make-vector k)
     (go:make (go:slice go:any) (go:as k go:int)))

   (define (not a)
     (boolean=? a #f))

   (define (null . o)
     (go:new: Null))

   (define (null? a)
     (or (m-null? a)
         (imm-null? a)))
         ;(and (imm-star? a) (exact=? (imm-star-length a) 0))))

   (define (object-equal? a b)
     (go:dot reflect (DeepEqual a b)))

;   (define (object-hash obj)
;     (go:when
;      (go:== (go:len rest) 1)
;      (go:return
;       ((go:dot (go:as (go:index rest 0) Hasher) GetHash))))
;     (go::= hashport (crc32.NewIEEE))
;     (go:for
;      (go::= i 0)
;      (go:< i (go:len rest))
;      (go:++ i)
;      (go::= code
;             (u32->bytevector
;              (object-hash (go:index rest i))))
;      (go::= (_ err)
;             (hashport.Write
;              (go:call (go:slice go:byte) (go:as code go:internal:binary))))
;      (go:when (go:!= err go:nil) (go:panic err)))
;     (go:return (go:uintptr (hashport.Sum32))))

   (define (pair? a)
     (or (m-pair? a)
         (imm-pair? a)
         (m-star? a)
         (imm-star? a)))
   ;(and (imm-list? a) (not (exact=? (imm-star-length a) 0)))))

;   (define (pointer-of a)
;     (go:dot reflect (ValueOf a) (Pointer)))
;
;   (define (pointer=? a b)
;     (go:== (go:as (pointer-of a) uintptr)
;            (go:as (pointer-of b) uintptr)))
;
;   (define (procedure? a)
;     (go::= (_ ok) (go:as a Proc))
;     ok)

   (define (s8? a)
     (go::= (_ ok) (go:as a go:int8))
     ok)

   (define (s16? a)
     (go::= (_ ok) (go:as a go:int16))
     ok)

   (define (s32? a)
     (go::= (_ ok) (go:as a go:int32))
     ok)

   (define (s64? a)
     (go::= (_ ok) (go:as a go:int64))
     ok)

   (define (string-append . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:return ""))
     ((1) (go:return (go:index rest 0))))
    ;; iterate
    (go::= strs (go:as (vector-map ->string rest) go:internal:vector))
    (go::= rl 0)
    (go:for (go::= i 0) (go:< i (go:len strs)) (go:++ i)
            (go:+= rl (go:len (go:as (go:index strs i) go:internal:string))))
    (go::= rv (go:as (make-string rl) go:internal:string))
    (go:for (go::= i 0) (go:< i (go:len strs)) (go:++ i)
      (go:= rv (go:apply go:append rv (go:as (go:index strs i) go:internal:string))))
    (go:return rv))

   (define (imm-string->utf8 str)
     (go:internal:binary (go:as str go:string)))

   (define (imm-string->string str)
     (go:internal:string (go:as str go:string)))

   (define (string->imm-string str)
     (go:string (go:as str go:internal:string)))

   (define (utf8->imm-string str)
     (go:string (go:as str go:internal:binary)))

   (define (string->utf8 str)
     (imm-string->utf8 (string->imm-string str)))

   (define (utf8->string str)
     (imm-string->string (utf8->imm-string str)))

;   (define (string . chars)
;     (vector->string chars))
;
;   (define (string->list a)
;     (vector->list (string->vector a)))
;
;   (define (string->symbol a)
;     (((go:dot a String) ToSymbol)))
;
;   (define (string->vector a)
;     (go::= st (go:as a go:string))
;     (go::= vc (go:as (make-vector (go:len st)) go:vector))
;     (go:for (go::= i 0) (< i (go:len st)) (++ i)
;             (go:= ((go:index vc i))
;                   (go:index st i)))
;     vc)
;
;   (define (string-hash a)
;     (go:dot (NewString (go:as a go:string)) (GetHash)))
;
;   (define (string? a)
;     (or (imm-string? a) 
;         (m-string? a)))

   ;(define (string->imm-string a)
   ;  (go:when* (go::= (s ok) (go:as a go:internal:string)) ok
   ;            (go:string s))
   ;  (error "expected string"))

   (define (symbol->imm-string a)
     (go:string (go:as a Symbol)))

   (define (symbol->string a)
     (imm-string->string (symbol->imm-string a)))

   (define (symbol=? a b)
     (unless (symbol? a) (type-error "symbol=?" a))
     (unless (symbol? b) (type-error "symbol=?" b))
     (go:== (go:as (symbol->string a) go:string)
            (go:as (symbol->string a) go:string)))

   (define (symbol? a)
     (go::= (_ ok) (go:as a Symbol))
     ok)

  (define (type-check expected . irrs)
    (go:range (go::= (_ irr) irrs)
              (unless (apply-vector expected (vector irr))
                      (type-error expected irr)))
    go:nil)

  (define (type-error expected irr)
    (raise
     (make-error-object
      (string-append
       "TypeError: expected " ;(procedure-name expected)
       " but got " ;(type-string irr)
       " instead.")
      irr)))

;   (define (type-error msg . irrs) (go:panic msg) (void))
;   (define (type-error msg . irrs)
;     (go:panic (go:as (string-append "TypeError in " msg) go:string)))

;   (define (type=? a b)
;     (go:== (go:as (kind-of a) go:uintptr)
;            (go:as (kind-of b) go:uintptr)))

   (define (u8-list->bytevector a)
     (u8-vector->bytevector (list->vector a)))
   
   (define (u8-vector->bytevector a)
     (go::= vc (go:as a go:internal:vector))
     (go::= bv (go:as (make-bytevector (go:len vc)) go:internal:binary))
     (go:for (go::= i 0) (go:< i (go:len vc)) (go:++ i)
             (go:= ((go:index bv i))
                   (go:as (go:index vc i) go:byte)))
     bv)

   (define (u8? a)
     (go::= (_ ok) (go:as a go:uint8))
     ok)

   (define (u16? a)
     (go::= (_ ok) (go:as a go:uint16))
     ok)

   (define (u32? a)
     (go::= (_ ok) (go:as a go:uint32))
     ok)

   (define (u64? a)
     (go::= (_ ok) (go:as a go:uint64))
     ok)

   (go:func make-values (k) (go:slice (go:dot reflect Value))
     (go:make (go:slice (go:dot reflect Value)) (go:as k go:int)))

   (go:func values rest (go:slice (go:dot reflect Value))
     (go::= rv (make-values (vector-length rest)))
     (go:for (go::= i 0) (go:< i (go:len rest)) (go:++ i)
             (go:= ((go:index rv i))
                   (object->value (go:index rest i))))
     rv)

   (define (vector . o)
     o)

   ;(define (vector . rest)
   ;  (apply list->vector rest))

   (define (vector->list a)
     (go::= (vc ls) (go:as a go:internal:vector) (null))
     (go:for (go::= i (go:- (go:len vc) 1)) (go:>= i 0) (go:-- i)
             (go:= ls (cons (go:index vc i) ls)))
     ls)

;   (define (vector->list* a)
;     (go::= vc (go:as a go:internal:vector))
;     (go:when (go:== (go:len vc) 0)
;              (go:return (null)))
;     (go:when (go:== (go:len vc) 1)
;              (go:return (go:index vc 0)))
;     (cons (go:index vc 0) (vector->list* (go:index vc 1))))

   (define (vector->string a)
     (go::= vc (go:as a go:internal:vector))
     (go::= st (go:as (make-string (go:len vc)) go:internal:string))
     (go:for (go::= i 0) (go:< i (go:len vc)) (go:++ i)
             (go:= ((go:index st i))
                   (go:as (go:index vc i) go:rune)))
     st)

   (define (vector-length vc)
     (go:len (go:as vc (go:slice (go:interface)))))

   (define (vector-map proc vc)
     (go::= sv (go:as vc go:internal:vector))
     (go::= sl (go:len sv))
     (go::= rv (go:as (make-vector sl) go:internal:vector))
     (go:for (go::= i 0) (go:< i sl) (go:++ i)
             (go:= ((go:index rv i))
                   (call proc (go:index sv i))))
     rv)

   (define (vector? a)
     (go::= (_ ok) (go:as a go:internal:vector))
     ok)

   (define (void)
     go:nil)

   (define (void? a)
     (go:== a go:nil))

;;;;;;;;;;;;;;;;;;;;; UNSORTED
;
;   (define (float32? a)
;     (go::= (_ ok) (go:as a go:float32))
;     ok)
;   (define (float64? a)
;     (go::= (_ ok) (go:as a go:float64))
;     ok)
;   (define (complex64? a)
;     (go::= (_ ok) (go:as a go:complex64))
;     ok)
;   (define (complex128? a)
;     (go::= (_ ok) (go:as a go:complex128))
;     ok)
;
;   ;; (error) is in error.go
;
;   ;(define (list*->vector pr)
;   ;  (go::= vec #((go:slice go:any)))
;   ;  (define-var (:: cur go:any))
;   ;  (go:for (go:= cur pr) (pair? (cdr cur)) (go:= cur (cdr cur))
;   ;       (go:= vec (go:append vec (car cur))))
;   ;  (go:= vec (go:append vec (car cur) (cdr cur)))
;   ;  (Vector vec))
;
;
;   ;(define (u32->bytevector a)
;   ;  (go::= buf (new bytes.Buffer))
;   ;  (go::= err
;   ;         (binary.Write buf binary.LittleEndian a))
;   ;  (go:when (go:!= err go:nil) (go:panic err))
;   ;  (go:return (Binary (buf.Bytes))))
;
;   ;(define (u8? a)
;   ;  (go:when* (go::= (_ ok) (go:as a byte)) ok
;   ;            (go:return #t))
;   ;  (go:when* (go::= (c ok) (go:as a int)) (and ok (<= 0 c 255))
;   ;            (go:return #t))
;   ;  #f)
;
;   (define (->go-string a)
;     (go:when* (go::= (s ok) (go:as a GoStringer)) ok
;               (go:return ((go:dot s GoString))))
;     (error "->go-string unknown type"))

   (define (->imm-string str)
     (go:case! (go:as str go:type)
      ((go:internal:string) (go:return (string->imm-string str)))
      ((go:string) (go:return str)))
     #f)

   ;(define (->imm-string a)
   ;  (go:when* (go::= (s ok) (go:as a go:string)) ok
   ;            (go:return s))
   ;  (go:when* (go::= (s ok) (go:as a (go:dot fmt Stringer))) ok
   ;            (go:return ((go:dot s String))))
   ;  (error "->imm-string unknown type"))

   (define (boolean->scheme-string a)
     (when a (go:return "#t"))
     "#f")

   (define (bytevector->scheme-string a) "#u8()")
   (define (scheme->scheme-string a) "\"\"")
   (define (vector->scheme-string a) "#()")
   (define (char->scheme-string a) "#\\c")
     ;(string-append "#\\" (list->string (list a))))

   (define (->scheme-string a)
     (when (void? a)
           (go:return "#<unspecified>"))
     (go:when* (go::= (ss ok) (go:as a SchemeStringer)) ok
               (go:return ((go:dot ss SchemeString))))
     (go:case! (go:as a go:type)
      ((go:bool) (boolean->scheme-string a))
      ((go:rune) (char->scheme-string a))
      ((Symbol) (symbol->string a)))
     (error "->scheme-string unknown type"))

   (define (->string str)
     (go:case! (go:as str go:type)
      ((go:internal:string) (go:return str))
      ((go:string) (go:return (imm-string->string str))))
     #f)

   ;(define (->string a)
   ;  (go:when* (go::= (s ok) (go:as a go:internal:string)) ok
   ;            (go:return s))
   ;  (imm-string->string (->imm-string a)))

   ;; odd-typed functions

   (go:func object->value (a) (go:dot reflect Value)
            ((go:dot reflect ValueOf) a))

   (go:func value->object (#(v (go:dot reflect Value))) go:any
            ((go:dot v Interface)))

   (go:func values->vector (#(vs (go:slice (go:dot reflect Value)))) go:any
            (go::= as (go:as (make-vector (go:len vs)) go:internal:vector))
            (go:for (go::= i 0) (go:< i (go:len vs)) (go:++ i)
                    (go:= ((go:index as i))
                          (value->object (go:index vs i))))
            (go:return as))


   (go:func vector->values (args) #(vs (go:slice (go:dot reflect Value)))
            (go::= as (go:as args (go:slice go:any)))
            (go:= vs (go:make (go:slice (go:dot reflect Value)) (go:len as)))
            (go:for (go::= i 0) (go:< i (go:len as)) (go:++ i)
                    (go:= ((go:index vs i))
                          (object->value (go:index as i))))
            (go:return))

   (go:func values->object (#(vs (go:slice (go:dot reflect Value)))) go:any
            (go:when (go:== (go:len vs) 1)
                     (go:return (go:as ((go:dot (go:index vs 0) Interface))
                                       go:any)))
            ;(go::= as
            ;       (make (go:slice go:any) (go:len vs)))
            ;(go:for
            ;  (go::= i 0)
            ;  (go:< i (go:len vs))
            ;  (go:++ i)
            ;  (go:= ((go:index as i))
            ;        (go:as ((go:dot (go:index vs i) Interface))
            ;               go:any)))
            ;(go:return (NewValues as))
            (go:panic "primitive procedures must have 1 return value")
            (go:return go:nil))

 )
)
