(define-library
 (ds scheme read)
 (import
  (only (ds scheme param)
        current-input-port
        current-output-port
        current-prompt
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
    (ReadLexer (newLexerWithState port state)))

  ;; read until end of file object
  (define (read-data port))

  ;; read until datum parses
  (define (read-with-prompt (port (current-input-port))
                            (output-port (current-output-port))
                            (prompt (current-prompt))))

  ;; read until expression parses
  (define (read-expression port  prompt))

  (define (read (port (current-input-port)))
    (go::= (value err) (Read (go:as port TIPort)))
    (go:when (go:!= err go:nil) (go:panic err))
    value)))

