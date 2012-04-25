(define-library (scheme lazy)
  (export
    delay ; derived
    eager ; derived
    force ; derived
    lazy) ; derived
  (import
   (scheme base))
  (begin

    ; These implementations come directly from 
    ; <http://srfi.schemers.org/srfi-45/srfi-45.html>
    ; with minor changes

	(define-syntax lazy
	  (syntax-rules ()
	    ((lazy exp) 
	     (list (cons 'lazy (lambda () exp))))))
	
	(define (eager x)
	  (list (cons 'eager x)))
	
	(define-syntax delay
	  (syntax-rules ()
	    ((delay exp)
         (lazy (eager exp)))))
	
	(define (force promise)
	  (let ((content (car promise)))
	    (case (car content)
	      ((eager) (cdr content))
	      ((lazy)  (let* ((promise* ((cdr content)))        
	                      (content  (car promise)))     
	                 (if (not (eqv? (car content) 'eager))
	                     (begin (set-car! content (car (car promise*)))
	                            (set-cdr! content (cdr (car promise*)))
	                            (set-car! promise* content)))
	                 (force promise))))))))
