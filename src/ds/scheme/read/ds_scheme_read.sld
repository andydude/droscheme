(define-library
  (ds scheme read)
  (export read)
  (begin
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
    (define (read-state #(state State) port)
      (ReadLexer (newLexerWithState port state)))
    (define (read (port (current-input-port)))
      (go::= (value err) (Read (go:as port TIPort)))
      (go:when (go:!= err go:nil) (go:panic err))
      value)))

