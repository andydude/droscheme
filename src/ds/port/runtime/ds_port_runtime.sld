; -*- mode: scheme -*-
(define-library (ds port runtime)
 (begin

   (define (standard-error-port)
     gStderr)

   (define (standard-input-port)
     gStdin)

   (define (standard-output-port)
     gStdout)

   (define (open-binary-input-file filename)
     (:= (file err) (dot os (Open filename)))
     (NewFilePort file filename PortTypeCodeByteIn))

   (define (open-binary-output-file filename)
     (:= (file err) (dot os (Create filename)))
     (NewFilePort file filename PortTypeCodeByteOut))

   (define (open-bytevector-input-port) (void))
   (define (open-bytevector-output-port) (void))
   (define (open-file-input-port) (void))
   (define (open-file-output-port) (void))
   (define (open-input-bytevector) (void))
   (define (open-input-file filename)
     (:= (file err) (dot os (Open filename)))
     (NewFilePort file filename PortTypeCodeCharIn))

   (define (open-input-string) (void))
   (define (open-output-bytevector) (void))
   (define (open-output-file filename)
     (:= (file err) (dot os (Create filename)))
     (NewFilePort file filename PortTypeCodeCharOut))

   (define (open-output-string) (void))
   (define (open-string-input-port) (void))
   (define (open-string-output-port) (void))

 );begin
);library
