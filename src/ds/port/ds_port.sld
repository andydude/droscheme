; -*- mode: scheme -*-
(define-library (ds port)
 (import
  "bufio"
  "os")
 (export
  binary-port?
  call-with-input-file
  call-with-output-file
  delete-file
  file-exists?
  input-port?
  open-binary-input-file
  open-binary-output-file
  open-input-file
  open-output-file
  output-port?
  port?
  standard-error-port
  standard-input-port
  standard-output-port
  textual-port?
  with-input-from-file
  with-output-to-file)
 (begin

   (define (standard-error-port)
     gStdErrorPort)

   (define (standard-input-port)
     gStdInputPort)

   (define (standard-output-port)
     gStdOutputPort)

   (define (open-binary-input-file filename (fileopt #f))
     (go:var #(file (go:ptr os.File)) 
             #(err go:error))
     (when (port? fileopt)
       (go:= file (go:as fileopt (go:ptr os.File))))
     (unless (port? fileopt)
       (go:= (file err) (os.Open (go:as filename go:string)))
       (go:when (go:!= err go:nil) (go:panic err)))
     (go::= port (go:new: FilePort
       (go:: name (go:as filename go:string))
       (go:: kind KindByteIn)))
     (go:= port.fl file)
     (go:= port.rd (bufio.NewReader port.fl))
     port)

   (define (open-binary-output-file filename (fileopt #f))
     (go:var #(file (go:ptr os.File)) 
             #(err go:error))
     (when (port? fileopt)
       (go:= file (go:as fileopt (go:ptr os.File))))
     (unless (port? fileopt)
       (go:= (file err) (os.Create (go:as filename go:string)))
       (go:when (go:!= err go:nil) (go:panic err)))
     (go::= port (go:new: FilePort
       (go:: name (go:as filename go:string))
       (go:: kind KindByteOut)))
     (go:= port.fl file)
     (go:= port.wr (bufio.NewWriter port.fl))
     port)

   (define (open-bytevector-input-port) #f)
   (define (open-bytevector-output-port) #f)
   (define (open-file-input-port) #f)
   (define (open-file-output-port) #f)
   (define (open-input-bytevector) #f)

   ;(define (open-input-file filename (fileopt #f))
   (define (open-input-file filename . opt)
     (go:var #(file (go:ptr os.File)) 
             #(err go:error))
     (go:case! (go:len opt)
       ((0) (go:= (file err) (os.Open (go:as filename go:string)))
            (go:when (go:!= err go:nil) (go:panic err)))
       ((1) (go:= file (go:as (go:index opt 0) (go:ptr os.File)))))
     (go::= port (go:new FilePort))
     (go:= port.name (go:as filename go:string))
     (go:= port.kind KindCharIn)
     (go:= port.fl file)
     (go:= port.rd (bufio.NewReader port.fl))
     port)

   (define (open-input-string) #f)
   (define (open-output-bytevector) #f)

   (define (open-output-file filename . opt)
     (go:var #(file (go:ptr os.File)) 
             #(err go:error))
     (go:case! (go:len opt)
       ((0) (go:= (file err) (os.Create (go:as filename go:string)))
            (go:when (go:!= err go:nil) (go:panic err)))
       ((1) (go:= file (go:as (go:index opt 0) (go:ptr os.File)))))
     (go::= port (go:new: FilePort
       (go:: name (go:as filename go:string))
       (go:: kind KindCharOut)))
     (go:= port.fl file)
     (go:= port.wr (bufio.NewWriter port.fl))
     port)

   (define (open-output-string) #f)
   (define (open-string-input-port) #f)
   (define (open-string-output-port) #f)

   (define (port? a)
     (go::= (_ ok) (go:as a PortKinder))
     ok)

   (define (binary-port? a)
     (go:when* (go::= (p ok) (go:as a PortKinder)) ok
       (go:case! (p.PortKind)
         ((KindByteIn) (go:return #t))
         ((KindByteOut) (go:return #t))
         ((KindByteInOut) (go:return #t))))
     #f)

   (define (textual-port? a)
     (go:when* (go::= (p ok) (go:as a PortKinder)) ok
       (go:case! (p.PortKind)
         ((KindCharIn) (go:return #t))
         ((KindCharOut) (go:return #t))
         ((KindCharInOut) (go:return #t))))
     #f)

   (define (output-port? a)
     (go:when* (go::= (p ok) (go:as a PortKinder)) ok
       (go:case! (p.PortKind)
         ((KindByteOut) (go:return #t))
         ((KindByteInOut) (go:return #t))
         ((KindCharOut) (go:return #t))
         ((KindCharInOut) (go:return #t))))
     #f)

   (define (input-port? a)
     (go:when* (go::= (p ok) (go:as a PortKinder)) ok
       (go:case! (p.PortKind)
         ((KindByteIn) (go:return #t))
         ((KindByteInOut) (go:return #t))
         ((KindCharIn) (go:return #t))
         ((KindCharInOut) (go:return #t))))
     #f)

 );begin
);library
