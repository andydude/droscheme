(define-library (scheme write)
  (export
    display
    write
    write-simple)
  (import
   (only
    (builtin)
    display
    write
    write-simple)))
