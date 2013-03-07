(define-library (ds any char)
 (import "unicode")
 (export
  char->integer
  char-alphabetic?
  char-bin-digit?
  char-ci<=?
  char-ci<?
  char-ci=?
  char-ci>=?
  char-ci>?
  char-digit?
  char-downcase
  char-foldcase
  char-hex-digit?
  char-lower-case?
  char-numeric?
  char-octal-digit?
  char-punctuation?
  char-symbolic?
  char-title-case?
  char-titlecase
  char-upcase
  char-upper-case?
  char-whitespace?
  char<=?
  char<?
  char=?
  char>=?
  char>?
  digit-value
  hex-digit-value
  integer->char)
 (begin

  (define (char->integer ch)
    (go:int64 (go:as ch go:rune)))

  (define (char-alphabetic? ch)
    ;(unless (char? ch)
    ;        (error "char-alphabetic? expected char"))
    (go::= cp (go:as ch go:rune))
    (unicode.IsLetter cp))

  (define (char-bin-digit? ch)
    (go::= cp (go:as ch go:rune))
    (go:or (go:== 48 cp) (go:== cp 49)))

  (define (char-ci<=? . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:<= (go:as (char-foldcase (go:index rest 0)) go:rune)
                            (go:as (char-foldcase (go:index rest 1)) go:rune)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
            (go:unless (go:<= (go:as (char-foldcase (go:index rest i)) go:rune)
                              (go:as (char-foldcase (go:index rest (go:+ i 1))) go:rune))
                       (go:return #f)))
    #t)

  (define (char-ci<? . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:< (go:as (char-foldcase (go:index rest 0)) go:rune)
                           (go:as (char-foldcase (go:index rest 1)) go:rune)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
            (go:unless (go:< (go:as (char-foldcase (go:index rest i)) go:rune)
                             (go:as (char-foldcase (go:index rest (go:+ i 1))) go:rune))
                       (go:return #f)))
    #t)

  (define (char-ci=? . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:== (go:as (char-foldcase (go:index rest 0)) go:rune)
                            (go:as (char-foldcase (go:index rest 1)) go:rune)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
            (go:unless (go:== (go:as (char-foldcase (go:index rest i)) go:rune)
                              (go:as (char-foldcase (go:index rest (go:+ i 1))) go:rune))
                       (go:return #f)))
    #t)

  (define (char-ci>=? . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:>= (go:as (char-foldcase (go:index rest 0)) go:rune)
                            (go:as (char-foldcase (go:index rest 1)) go:rune)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
            (go:unless (go:>= (go:as (char-foldcase (go:index rest i)) go:rune)
                              (go:as (char-foldcase (go:index rest (go:+ i 1))) go:rune))
                       (go:return #f)))
    #t)

  (define (char-ci>? . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:> (go:as (char-foldcase (go:index rest 0)) go:rune)
                           (go:as (char-foldcase (go:index rest 1)) go:rune)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
            (go:unless (go:> (go:as (char-foldcase (go:index rest i)) go:rune)
                             (go:as (char-foldcase (go:index rest (go:+ i 1))) go:rune))
                       (go:return #f)))
    #t)

  (define (char-digit? ch)
    ;(unless (char? ch)
    ;        (error "char-alphabetic? expected char"))
    (go::= cp (go:as ch go:rune))
    (unicode.IsDigit cp))

  ;(define (char-general-category ch))

  (define (char-downcase ch)
    (go::= cp (go:as ch go:rune))
    (unicode.ToLower cp))

  (define (char-foldcase ch)
    (char-downcase (char-upcase ch)))

  (define (char-hex-digit? ch)
    ;(unless (char? ch)
    ;        (error "char-alphabetic? expected char"))
    (go::= cp (go:as ch go:rune))
    (unicode.IsOneOf (go:make: (go:slice (go:ptr unicode.RangeTable)) unicode.Hex_Digit) cp))

  (define (char-lower-case? ch)
    (go::= cp (go:as ch go:rune))
    (unicode.IsLower cp))

  (define (char-numeric? ch)
    ;(unless (char? ch)
    ;        (error "char-alphabetic? expected char"))
    (go::= cp (go:as ch go:rune))
    (unicode.IsNumber cp))

  (define (char-octal-digit? ch)
    (go::= cp (go:as ch go:rune))
    (go:and (go:<= 48 cp) (go:<= 55 cp)))

  (define (char-punctuation? ch)
    (go::= cp (go:as ch go:rune))
    (unicode.IsPunct cp))

  (define (char-symbolic? ch)
    (go::= cp (go:as ch go:rune))
    (unicode.IsSymbol cp))

  (define (char-title-case? ch)
    (go::= cp (go:as ch go:rune))
    (unicode.IsTitle cp))

  (define (char-titlecase ch)
    (go::= cp (go:as ch go:rune))
    (unicode.ToTitle cp))

  (define (char-upcase ch)
    (go::= cp (go:as ch go:rune))
    (unicode.ToUpper cp))

  (define (char-upper-case? ch)
    (go::= cp (go:as ch go:rune))
    (unicode.IsUpper cp))

  (define (char-whitespace? ch)
    (go::= cp (go:as ch go:rune))
    (unicode.IsSpace cp))

  (define (char<=? . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:<= (go:as (go:index rest 0) go:rune)
                           (go:as (go:index rest 1) go:rune)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
            (go:unless (go:<= (go:as (go:index rest i) go:rune)
                              (go:as (go:index rest (go:+ i 1)) go:rune))
                       (go:return #f)))
    #t)

  (define (char<? . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:< (go:as (go:index rest 0) go:rune)
                           (go:as (go:index rest 1) go:rune)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
            (go:unless (go:< (go:as (go:index rest i) go:rune)
                             (go:as (go:index rest (go:+ i 1)) go:rune))
                       (go:return #f)))
    #t)

  (define (char=? . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:== (go:as (go:index rest 0) go:rune)
                            (go:as (go:index rest 1) go:rune)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
            (go:unless (go:== (go:as (go:index rest i) go:rune)
                              (go:as (go:index rest (go:+ i 1)) go:rune))
                       (go:return #f)))
    #t)

  (define (char>=? . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:>= (go:as (go:index rest 0) go:rune)
                            (go:as (go:index rest 1) go:rune)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
            (go:unless (go:>= (go:as (go:index rest i) go:rune)
                              (go:as (go:index rest (go:+ i 1)) go:rune))
                       (go:return #f)))
    #t)

  (define (char>? . rest)
    ;; fast
    (go:case! (go:len rest)
     ((0) (go:panic "no arguments"))
     ((1) (go:panic "nothing to compare to"))
     ((2) (go:return (go:> (go:as (go:index rest 0) go:rune)
                           (go:as (go:index rest 1) go:rune)))))
    ;; iterate
    (go:for (go::= i 0) (go:< i (go:- (go:len rest) 1)) (go:++ i)
            (go:unless (go:> (go:as (go:index rest i) go:rune)
                             (go:as (go:index rest (go:+ i 1)) go:rune))
                       (go:return #f)))
    #t)

  (define (char? a)
    (go::= (_ ok) (go:as a go:rune))
    ok)

  (define (digit-value ch)
    ;(unless (char-digit? ch)
    ;        (error "digit-value expected char"))
    (go::= cp (go:as ch go:rune))
    (go:when (go:and (go:<= 48 cp) (go:<= cp 57))
             (go:return (go:- (go:int64 cp) 48)))
    #f)

  (define (hex-digit-value ch)
    (go::= cp (go:as ch go:rune))
    (go:when (go:and (go:<= 48 cp) (go:<= cp 57))
             (go:return (go:- (go:int64 cp) 48)))
    (go:when (go:and (go:<= 65 cp) (go:<= cp 70))
             (go:return (go:- (go:int64 cp) 55)))
    (go:when (go:and (go:<= 97 cp) (go:<= cp 102))
             (go:return (go:- (go:int64 cp) 87)))
    #f)

  (define (integer->char cp)
    (go:rune (go:as cp go:int64)))

 );begin
);package
