(define-library (scheme complex)
  (export
    angle
    imag-part
    magnitude
    make-polar
    make-rectangular
    real-part)
  (import
   (builtin))
  (begin

    (define-record-type complex-rect
      make-rectangular complex-rect?
      (re complex-rect-real)
      (im complex-rect-imag))

    (define-record-type complex-polar
      make-polar complex-polar?
      (mag complex-polar-magnitude)
      (ang complex-polar-angle))

    (define (complex-polar-real z)
      (let ((r (complex-polar-magnitude z))
            (a (complex-polar-angle z)))
        (* r (cos a))))

    (define (complex-polar-imag z)
      (let ((r (complex-polar-magnitude z))
            (a (complex-polar-angle z)))
        (* r (sin a))))

    (define (complex-rect-magnitude z)
      (define (sq x) (* x x))
      (define (hypot x y) (sqrt (+ (sq x) (sq y))))
      (let ((x (complex-rect-real z))
            (y (complex-rect-imag z)))
        (hypot x y))) ; this should be builtin
            
    (define (complex-rect-angle z)
      (let ((x (complex-rect-real z))
            (y (complex-rect-imag z)))
        (atan2 x y)))

    (define (real-part z)
      (cond
       ((complex-polar? z) (complex-polar-real z))
       ((complex-rect? z) (complex-rect-real z))
       ((real? z) z)))

    (define (imag-part z)
      (cond
       ((complex-polar? z) (complex-polar-imag z))
       ((complex-rect? z) (complex-rect-imag z))
       ((real? z) 0)))

    (define (magnitude z)
      (cond
       ((complex-polar? z) (complex-polar-magnitude z))
       ((complex-rect? z) (complex-rect-magnitude z))
       ((real? z) z)))

    (define (angle z)
      (cond
       ((complex-polar? z) (complex-polar-angle z))
       ((complex-rect? z) (complex-rect-angle z))
       ((real? z) 0)))))