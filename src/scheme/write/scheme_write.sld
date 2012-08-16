(define-library (scheme write)
  (export
    display
    write
    write-simple)
  (import
   (only
    (ds base)
    display
    write
    write-simple)))
