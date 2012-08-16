#lang racket

(require srfi/13)
(require srfi/98)

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
    ((#\.) "ZL")
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
    ((#\L) ".")
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

;;; not in racket
;(define (string-null? str)
;  (equal? str ""))

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

; simplify parameters

;(define (string-replace str c s)
;  (regexp-replace c str s))

;; TODO: don't ask...
(define (string-escape a)
  (let* ((b (string-replace a "\\\\" "\\\\\\\\"))
         (c (string-replace b "\\\"" "\\\\\\\""))
         (d (string-replace c "\n" "\\\\n")))
    d))

(define list->eqv-hashtable make-hasheqv)
(define hashtable->list hash->list)
(define hashtable-contains? hash-has-key?)
(define hashtable-ref hash-ref)
(define hashtable-set! hash-set!)
