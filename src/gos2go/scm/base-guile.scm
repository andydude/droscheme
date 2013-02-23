(use-modules (ice-9 regex)
             (srfi srfi-39))

(define (most ls)
  (if (null? ls) '()
      (if (null? (cdr ls)) '()
          (cons (car ls) (most (cdr ls))))))

(define (last ls)
  (if (null? ls) ls
      (if (null? (cdr ls)) (car ls)
          (last (cdr ls)))))

; srfi-98
(define get-environment-variable getenv)
;(define (get-environment-variables)
;  do something to environ)

(define list->eqv-hashtable values)

(define hashtable->list values)

(define (hashtable-contains? alist key)
  (not (not (assoc key alist))))

(define (hashtable-ref alist key)
  (cdr (assoc key alist)))

(define (hashtable-set! alist key value)
  '())

(define string-subcopy string-replace)

;; TODO
(define (string-replace str c s)
  (if (not str)
      str
      (regexp-substitute/global #f
       c str 'pre s 'post)))
;      (regexp-substitute/global #f
;        (string-match c str)
;        'pre s 'post)))

;; TODO: don't ask...
(define (string-escape z)
  (let* ((a (string-replace z "\\\\" "\\\\"))
         (b (string-replace a "\\\"" "\\\""))
         ;(c (string-replace b "\x00" "\\x00"))
         (d (string-replace b "\x07" "\\a"))
         (e (string-replace d "\x08" "\\b"))
         (f (string-replace e "\x0B" "\\v"))
         (g (string-replace f "\x0C" "\\f"))
         (h (string-replace g "\t" "\\t"))
         (i (string-replace h "\r" "\\r"))
         (j (string-replace i "\n" "\\n")))
    j))

(define (keyword->string kw)
  (symbol->string (keyword->symbol kw)))

(define (string->keyword kw)
  (symbol->keyword (string->symbol kw)))

(define (vector-first vc)
  (vector-ref vc 0))

(define (vector-rest vc)
  (list->vector (cdr (vector->list vc))))

(define (vector-most vc)
  (list->vector (reverse (cdr (reverse (vector->list vc))))))

(define (vector-last vc)
  (vector-ref vc (- (vector-length vc) 1)))

;; not in guile
(define (string-null? str)
  (equal? str ""))
