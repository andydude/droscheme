(define-library
 (ds scheme read)
 (import
  (ds any)
  (ds any error)
  (ds port)
  (only (ds scheme parameter)
        current-input-port
        current-output-port
        current-prompt))
 (export
  read-data
  read-lines
  read)
 (begin

  ;; private
  (go:func
    read-lexer
    (#(lex (go:ptr Lexer)))
    (values #(value go:any) #(err go:error))
    (yyParse lex)
    (go:when
      (go:== lex.pcount 0)
      (go:= err lex.err)
      (go:else (go:= err gEOL)))
    (go:= value lex.value)
    (go:return))

  ;; private
  (define (read-state #(state State) port)
    (go::= (value err) (read-lexer (newLexerWithState (go:as port ds_port.TIPort) state)))
    (go:when (go:!= err go:nil) (go:panic err))
    value)

  ;; read until end of file object
  (define (read-data port)
    (read port))

  ;; read until datum parses
  (define (read-with-prompt (prompt (current-prompt))
                            (port (current-input-port))
                            (output-port (current-output-port)))
    (read port))

  ;; read until expression parses
  (define (read-expression port prompt))

  (define (read-lines port)
    (read-with-prompt "    " port (current-output-port)))

  (define (read (port (current-input-port)))
    (go::= (value err) (read-lexer (newLexer (go:as port ds_port.TIPort))))
    (go:when (go:!= err go:nil) (go:panic err))
    value)

 )
)
