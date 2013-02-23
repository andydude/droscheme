(define-library
 (ds seq list cadr)
 (import
  (only (ds seq runtime) car cdr))
 (export
  caar
  cadr
  caaar
  caadr
  cadar
  caddr
  caaaar
  caaadr
  caadar
  caaddr
  cadaar
  cadadr
  caddar
  cadddr
  cdaaar
  cdaadr
  cdadar
  cdaddr
  cddaar
  cddadr
  cdddar
  cddddr
  cdaar
  cdadr
  cddar
  cdddr
  cdar
  cddr)
 (begin

  (define (caar ls) (car (car ls)))
  (define (cadr ls) (car (cdr ls)))
  (define (caaar ls) (car (cdr (cdr ls))))
  (define (caadr ls) (car (cdr (cdr ls))))
  (define (cadar ls) (car (cdr (cdr ls))))
  (define (caddr ls) (car (cdr (cdr ls))))
  (define (caaaar ls) (car (car (car (car ls)))))
  (define (caaadr ls) (car (car (car (cdr ls)))))
  (define (caadar ls) (car (car (cdr (car ls)))))
  (define (caaddr ls) (car (car (cdr (cdr ls)))))
  (define (cadaar ls) (car (cdr (car (car ls)))))
  (define (cadadr ls) (car (cdr (car (cdr ls)))))
  (define (caddar ls) (car (cdr (cdr (car ls)))))
  (define (cadddr ls) (car (cdr (cdr (cdr ls)))))
  (define (cdaaar ls) (cdr (car (car (car ls)))))
  (define (cdaadr ls) (cdr (car (car (cdr ls)))))
  (define (cdadar ls) (cdr (car (cdr (car ls)))))
  (define (cdaddr ls) (cdr (car (cdr (cdr ls)))))
  (define (cddaar ls) (cdr (cdr (car (car ls)))))
  (define (cddadr ls) (cdr (cdr (car (cdr ls)))))
  (define (cdddar ls) (cdr (cdr (cdr (car ls)))))
  (define (cddddr ls) (cdr (cdr (cdr (cdr ls)))))
  (define (cdaar ls) (cdr (car (car ls))))
  (define (cdadr ls) (cdr (car (cdr ls))))
  (define (cddar ls) (cdr (cdr (car ls))))
  (define (cdddr ls) (cdr (cdr (cdr ls))))
  (define (cdar ls) (cdr (car ls)))
  (define (cddr ls) (cdr (cdr ls)))

 )
)
