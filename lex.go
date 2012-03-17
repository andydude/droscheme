package droscheme

import (
	"fmt"
	"strconv"
	"strings"
	"unicode/utf8"
)

const eof = 0x7F
var base int = 10
var inexact bool = false

// [AJR] Yes, I watched Rob Pike
type State func(*Lexer) State
type Lexer struct {
	input string
	start, pos, width int
	tokens chan *yySymType
	state State
	ch rune
}

func newLexer(input string) *Lexer {
	const bufferSize = 5
	lex := &Lexer{
		input: input, 
		tokens: make(chan *yySymType, bufferSize),
		state: (*Lexer).lexToken,
		ch: rune(input[0]),
	}
	return lex
}

func newToken(token int, datum Any) *yySymType {
	tok := &yySymType{
		token: token,
		datum: datum,
	}
	return tok
}

func newEOF() *yySymType {
	return newToken(-1, nil)
}

func (t *yySymType) String() string {
	return fmt.Sprintf("%s[%d]", t.datum, t.token)
}

//
// lexer methods
//

func (lex *Lexer) emitDatum(token int, datum Any) {
	lex.tokens <- newToken(token, datum)
	lex.consume()
}

func (lex *Lexer) emitId(name string) {
	lex.emitDatum(ID, SSymbol{name})
}

func (lex *Lexer) emit(token int) {
	lex.emitDatum(token, nil)
}

func (lex *Lexer) accept1(valid rune) bool {
	if lex.next() == valid {
		return true
	}
	lex.backup()
	return false
}

func (lex *Lexer) acceptN(valid string) {
	for strings.IndexRune(valid, lex.next()) >= 0 {
	}
	lex.backup()
}

func (lex *Lexer) accept(valid string) bool {
	if strings.IndexRune(valid, lex.next()) >= 0 {
		return true
	}
	lex.backup()
	return false
}

func (lex *Lexer) backup() {
	//fmt.Printf("\n-- backup() --\n")
	lex.pos -= lex.width
}

func (lex *Lexer) consume() {
	lex.start = lex.pos
}

func (lex *Lexer) match1(valid rune) {
	if lex.next() != valid {
		panic("failed to match")
	}
}

func (lex *Lexer) match(valid string) {
	if valid == "" { return }
	if lex.next() != rune(valid[0]) {
		panic("failed to match")
	}
	lex.match(valid[1:])
}

func (lex *Lexer) nextToken() *yySymType {
	//fmt.Printf("\n-- nextyySymType() --\n")
	for {
		select {
		case tok := <- lex.tokens:
			//fmt.Printf("\n-- nextToken() %s --\n", tok.String())
			return tok
		default:
			if lex.state == nil {
				
				return newEOF()
			}
			lex.state = lex.state(lex)
		}
	}
	panic("unreachable")
}

// this is like consume() and eat()
func (lex *Lexer) next() rune {
	if lex.pos >= len(lex.input) {
		lex.width = 0
		lex.ch = eof
	} else {
		lex.ch, lex.width = utf8.DecodeRuneInString(lex.input[lex.pos:])
		lex.pos += lex.width
	}
	//fmt.Printf("\n-- next() %s--\n", string(lex.ch))
	return lex.ch
}

func (lex *Lexer) peek() rune {
	//fmt.Printf("\n-- peek() --\n")
	ch := lex.next()
	lex.backup()
	return ch
}

//func (lex *Lexer) run() {
//	for state := (*Lexer).lexToken; state != nil; {
//		state = state(lex)
//	}
//	close(lex.tokens)
//}

func (lex *Lexer) skip() {
	//fmt.Printf("\n-- skip() --\n")
	lex.next()
}

//
// grammar methods
//

func (lex *Lexer) lexSpace() State {
	for lex.isWhitespace() {
		lex.skip()
	}
	lex.backup()
	lex.start = lex.pos
	return (*Lexer).lexToken
}

func (lex *Lexer) lexToken() State {
	switch r := lex.peek(); {
	case r == eof: return nil
	case r == '"': return (*Lexer).lexString
	case r == '#': return (*Lexer).lexHash
	case r == '.': return (*Lexer).lexDot
	case r == '\'': lex.next(); lex.emit(QUOTE)
	case r == '`': lex.next(); lex.emit(QQUOTE)
	case r == ',':
		lex.next()
		if lex.peek() == '@' {
			lex.next()
			lex.emit(UNQUOTES)
		} else {
			lex.emit(UNQUOTE)
		}
	case r == '+' || r == '-':
		return (*Lexer).lexSigns
	case lex.isInitial():
		return (*Lexer).lexId
	case lex.isNumeric():
		base = 10
		return (*Lexer).lexNumber
	case lex.isWhitespace():
		return (*Lexer).lexSpace
	default:
		lex.emit(int(lex.next()))
	}

	return (*Lexer).lexToken
}

// ⟨string⟩ -> " ⟨string element⟩* "
func (lex *Lexer) lexString() State {
	lex.match1('"')

	// not using lex.input[start:pos] to lex the string because
	// we want to skip escape characters, not include them in
	// the string contents
	contents := []rune{}
	
	lex.next()
	for lex.isStringElement() {
		//fmt.Printf("lex.ch == %c\n", lex.ch)
		contents = append(contents, lex.ch)
		lex.next()
	}
	lex.backup()
	lex.match1('"')

	lex.emitDatum(STRING, SString{string(contents)})
	return (*Lexer).lexToken
}

func (lex *Lexer) lexChar() State {
	// assume we've consumed #\ already
	ch := lex.next()
	if ch == 'x' {
		// TODO
	} else {
		lex.emitDatum(CHAR, SChar(ch))
	}
	return (*Lexer).lexToken
}

func (lex *Lexer) lexDot() State {
	// can start the following tokens:
	//
	// <identifier>
	//   <peculiar identifier> = ...
	// <number>
	//   <decimal 10> = . digit+ suffix
	// . as in
	//   <list> = ( datum+ . datum )
	if lex.next() != '.' {
		panic("unreachable")
	}

	//fmt.Printf("\n-- lexDot() --\n")

	if lex.next() == '.' {
		if lex.next() == '.' {
			lex.emitId("...")
			return (*Lexer).lexToken
		} else {
			panic("expected ...")
		}
	} else if lex.isNumeric() {
		return (*Lexer).lexNumber
	}

	// assume id
	lex.emit('.')
	return (*Lexer).lexSpace
}

func (lex *Lexer) lexHash() State {
	// can start the following tokens:
	// #! comment
	// #| comment |#
	// #; comment
	// #\ character -- done (TODO: \x)
	// #' syntax
	// #` quasisyntax
	// #, unsyntax
	// #,@ unsyntax-splicing
	// #tf boolean
	// #( vector )
	// #u8( u8vector )
	// #vu8( u8vector )
	// #iebodx number
	lex.next() // #

	//fmt.Printf("\n-- lexHash() --\n")

	switch r := lex.next(); {
	case r == '\'':
		lex.emit(SYNTAX)
		return (*Lexer).lexToken
	case r == '`':
		lex.emit(QSYNTAX)
		return (*Lexer).lexToken
	case r == ',':
		if lex.peek() == '@' {
			lex.next()
			lex.emit(UNSYNTAXS)
		} else {
			lex.emit(UNSYNTAX)
		}
		return (*Lexer).lexToken
	case r == '\\':
		return (*Lexer).lexChar
	case r == 'f':
		lex.emitDatum(BOOL, SBool(false))
		return (*Lexer).lexToken
	case r == 't':
		lex.emitDatum(BOOL, SBool(true))
		return (*Lexer).lexToken
	case r == 'v':
		lex.next()
		fallthrough
	case r == 'u':
		lex.match1('8')
		lex.match1('(')
		lex.emit(U8VECTORPAREN)
		return (*Lexer).lexToken
	case r == '(':
		lex.emit(VECTORPAREN)
		return (*Lexer).lexToken
	case r == 'e':
		inexact = false
		return (*Lexer).lexNumber
	case r == 'i':
		inexact = true
		return (*Lexer).lexNumber
	case r == 'b':
		base = 2
		return (*Lexer).lexNumber
	case r == 'o':
		base = 8
		return (*Lexer).lexNumber
	case r == 'd':
		base = 10
		return (*Lexer).lexNumber
	case r == 'x':
		base = 16
		return (*Lexer).lexNumber
	}

	// readtable support could go here

	return nil
}

func (lex *Lexer) lexSigns() State {
	// can start the following tokens:
	//
	// <identifier>
	//   <peculiar identifier> = + | - | -> subsequent*
	// <number>
	//   <complex 10> = +i
	//   <real 10> = +inf.0
	//   <real 10> = +nan.0
	s := lex.next() 
	if s != '+' && s != '-'  {
		panic("unreachable")
	}
	r := lex.peek()
	if s == '-' && r == '>' {
		lex.pos += 1
		return (*Lexer).lexId
	}

	//fmt.Printf("\n-- lexSigns() %s--\n", string(s))

	// if this is the beginning of the token, then it is a number
	switch {
	case r == 'i': // inf.0 or i
		fallthrough
	case r == 'n': // nan.0
		fallthrough
	case lex.isNumeric():
		base = 10
		lex.backup()
		return (*Lexer).lexNumber
	}

	// if this is the end of the token, then it is an id
	lex.pos += 1
	return (*Lexer).lexId
}

// <identifier>
func (lex *Lexer) lexId() State {
	for lex.isSubsequent() { lex.next() }
	lex.backup()
	str := lex.input[lex.start:lex.pos]
	//fmt.Printf("\n-- lexId() %s--\n", str)
	lex.emitId(str)
	return (*Lexer).lexToken
}

// <number>
func (lex *Lexer) lexNumber() State {
	//lex.accept1('+')
	//lex.accept1('-')
	for lex.isNumeric() { lex.next() }
	lex.backup()

	// TODO: rest of number syntax
	num := lex.input[lex.start:lex.pos]
	//fmt.Printf("\n-- lexNumber() %s--\n", num)
	x, err := strconv.Atoi(num)
	if err != nil {
		panic(err)
		panic("Invalid number")
	}
	lex.emitDatum(NUMBER, Sint64(x))
	return (*Lexer).lexToken
}

// character class methods

func (lex *Lexer) isLetter() bool {
	//fmt.Printf("\n-- isLetter() --\n")
	return 'a' <= lex.ch && lex.ch <= 'z' || 'A' <= lex.ch && lex.ch <= 'Z'
}

func (lex *Lexer) isNumeric() bool {
	//fmt.Printf("\n-- isNumeric() --\n")
	return '0' <= lex.ch && lex.ch <= '9'
}

func (lex *Lexer) isInitial() bool {
	//fmt.Printf("\n-- isInitial() --\n")
	return lex.isLetter() || lex.isSpecialInitial()
}

func (lex *Lexer) isSubsequent() bool {
	return lex.isInitial() || lex.isNumeric() || lex.isSpecialSubsequent()
}

func (lex *Lexer) isSpecialInitial() bool {
	switch lex.ch {
	case '!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '^', '_', '~':
		return true
	}
	return false
}

func (lex *Lexer) isSpecialSubsequent() bool {
	return lex.ch == '.' || lex.ch == '+' || lex.ch == '-'
}

func (lex *Lexer) isWhitespace() bool {
	if lex.ch == ' ' || lex.ch == '\t' || lex.ch == '\n' || lex.ch == 'r' {
		return true
	}
	return false
}

// ⟨string element⟩ -> ⟨any character other than " or \⟩ | \" | \\
func (lex *Lexer) isStringElement() bool {
	if lex.ch == '"' {
		return false
	}
	if lex.ch == '\\' {
		lex.next()
		if lex.ch == '\\' || lex.ch == '"' {
			return true
		}
		// not a \\ or \"--put it back
		lex.backup()
		return false
	}
	return true
}
