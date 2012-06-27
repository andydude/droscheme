//
// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins, Daniel Connelly
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
//
package droscheme

import (
	"io"
	"fmt"
	"math"
	"math/big"
	"strconv"
	"strings"
	//"runtime/debug"
)

const eof = rune(-1)

// [AJR] Yes, I watched Rob Pike
type State func(*Lexer) State
type Lexer struct {
	buffer            []rune
	input             TIPort
	start, pos, width int
	tokens            chan *yySymType
	state             State
	ch                rune
	pcount            int
	base, sign        int
    inexact           bool
    err               error
	value             Any
}

//func newLexer(input string) *Lexer {
//	return newLexerFromPort(OpenTIPort(input).(TIPort))
//}
//
//func newLexerFile(filename string) *Lexer {
//	return newLexerFromPort(OpenTIFile(filename).(TIPort))
//}

//func newBufReaderForLexer(input string) *dsbufio.Reader {
//	return dsbufio.NewReader(newReaderForLexer(input))
//}

//func newReaderForLexer(input string) io.Reader {
//	port := &SBytePort{
//		it:   NewBinary([]byte(input)),
//		code: PortTypeCodeByteIn}
//
//	return port
//}

func emptyTokens() chan *yySymType {
	return make(chan *yySymType, 4)
}

func newLexer(rd TIPort) *Lexer {
	return newLexerWithState(rd, (*Lexer).lexToken)
}

func newLexerWithBase(rd TIPort, base int) *Lexer {
	lex := &Lexer{
	    buffer: []rune{},
		input:  rd,
		tokens: emptyTokens(),
		state:  (*Lexer).lexNumber,
		base:   base,
		sign:   1,
		ch:     0}

	return lex
}

func newLexerWithState(rd TIPort, state State) *Lexer {
	lex := &Lexer{
	    buffer: []rune{},
		input:  rd,
		tokens: emptyTokens(),
		state:  state,
		base:   10,
		sign:   1,
		ch:     0}

	return lex
}

func newToken(token int, datum Any) *yySymType {
	tok := &yySymType{
		token: token,
		datum: datum,
	}
	return tok
}

func (t *yySymType) String() string {
	return fmt.Sprintf("%s[%d]", t.datum, t.token)
}

//
// lexer methods
//

func (lex *Lexer) emitLabel(label int) {
	lex.emitDatum(LABEL, NewLabel(Sint64(label), nil))
}

func (lex *Lexer) emitDatum(token int, datum Any) {
	//fmt.Printf("emit(%d, %v)\n", token, datum)
	lex.tokens <- newToken(token, datum)
	lex.consume()
}

func (lex *Lexer) emitNum(datum Any) {
	lex.emitDatum(NUMBER, datum)
}

func (lex *Lexer) emitId(name string) {
	lex.emitDatum(ID, SSymbol{name})
}

func (lex *Lexer) emit(token int) {
	lex.emitDatum(token, nil)
}

func (lex *Lexer) accept1(valid rune) bool {
	if lex.peek() == valid {
		lex.next()
		return true
	}
	return false
}

func (lex *Lexer) accept(valid string) bool {
	for strings.IndexRune(valid, lex.peek()) >= 0 {
		lex.next()
		return true
	}
	return false
}

func (lex *Lexer) backup() {
	defer func() {
		x := recover()
		if x != nil {
			fmt.Printf("backup s=%s, p=%s, buf=%s\n", lex.start, lex.pos, lex.buffer)
			panic("error in backup()")
		}
	}()
	lex.pos -= lex.width
	if len(lex.buffer) > 0 {
		lex.buffer = lex.buffer[0:len(lex.buffer) - 1]
	}
	error1panic(lex.input.UnreadRune())
}

func (lex *Lexer) consume() {
	lex.start = lex.pos
	lex.buffer = []rune{}
}

func (lex *Lexer) match1(valid rune) {
	if lex.next() != valid {
		panic("failed to match: " + string([]rune{valid}))
	}
}

func (lex *Lexer) match(valid string) {
	if valid == "" {
		return
	}
	if lex.next() != rune(valid[0]) {
		panic("failed to match")
	}
	lex.match(valid[1:])
}

func (lex *Lexer) getSpan() string {
	defer func() {
		x := recover()
		if x != nil {
			fmt.Printf("s=%s, p=%s, w=%s\n", lex.start, lex.pos, lex.width)
			panic("error in getSpan()")
		}
	}()
	//return lex.input[lex.start:lex.pos]
	return string(lex.buffer)
}

func (lex *Lexer) nextToken() *yySymType {
	for {
		select {
		case tok := <-lex.tokens:
			return tok
		default:
			if lex.state == nil {
				return newToken(-1, nil)
			}
			lex.state = lex.state(lex)
		}
	}
	panic("unreachable")
}

// this is like consume() and eat()
func (lex *Lexer) next() rune {
	const debug = false
	if lex.ch == eof {
		lex.err = io.EOF
		lex.consume()
		//fmt.Printf("--- next(%d) = '%s'\n", lex.ch, lex.buffer)
		return eof
	}
	var err error
	lex.ch, lex.width, err = lex.input.ReadRune()
	if err != nil {
		if err != io.EOF {
			panic(err)
		}
		lex.ch, lex.width = eof, 0
		if debug {
			fmt.Printf("--- next(%d) = '%s'\n", lex.ch, lex.buffer)
		}
		return lex.ch
	}
	lex.buffer = append(lex.buffer, lex.ch)
	lex.pos += lex.width
	if debug {
		fmt.Printf("--- next(%d) = '%s'\n", lex.ch, lex.buffer)
	}
	return lex.ch
}

func (lex *Lexer) peek() rune {
	const debug = false
	if lex.ch == eof {
		//fmt.Printf("--- peek(%d)\n", eof)
		return eof
	}
	var err error
	lex.ch, lex.width, err = lex.input.PeekRune()
	if err != nil {
		if err != io.EOF {
			panic(err)
		}
		lex.ch, lex.width = eof, 0
	}
	if debug {
		fmt.Printf("--- peek(%d)\n", lex.ch)
	}
	return lex.ch
}

func (lex *Lexer) skip() {
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
	lex.consume()
	lex.base = 10
	lex.sign = 1
	return (*Lexer).lexToken
}

func (lex *Lexer) lexToken() State {
	switch r := lex.peek(); {
	case r == 0:
		return nil
	case r == eof:
		return nil
	case r == ';':
		return (*Lexer).lexLineComment
	case r == '"':
		return (*Lexer).lexString
	case r == '#':
		return (*Lexer).lexHash
	case r == '.':
		return (*Lexer).lexDot
	case r == '\'':
		lex.next()
		lex.emit(QUOTE)
	case r == '`':
		lex.next()
		lex.emit(QQUOTE)
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
	case lex.isDigit10():
		lex.base = 10
		return (*Lexer).lexNumber
	case lex.isWhitespace():
		return (*Lexer).lexSpace
	case r == '(':
		lex.pcount++
		lex.emit(int(lex.next()))
	case r == ')':
		lex.pcount--
		lex.emit(int(lex.next()))
		if lex.pcount == 0 {
			return nil
		}
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
		if lex.ch == '\\' {
			lex.next()
			switch lex.ch {
			case 'a':
				lex.ch = 0x07
			case 'b':
				lex.ch = 0x08
			case 't':
				lex.ch = 0x09
			case 'n':
				lex.ch = 0x0A
			case 'v':
				lex.ch = 0x0B
			case 'f':
				lex.ch = 0x0C
			case 'r':
				lex.ch = 0x0D
			case 'x':
				lex.peek()
				lex.consume()
				for lex.isDigit16() {
					lex.next()
				}
				lex.backup()
				lex.base = 16
				ret := lex.getInt().(Sint64)
				lex.match1(';')
				lex.ch = rune(ret)
			}
		}
		contents = append(contents, lex.ch)
		lex.next()
	}
	lex.backup()
	lex.match1('"')

	lex.emitDatum(STRING, NewString(contents))
	return (*Lexer).lexToken
}

func (lex *Lexer) lexChar() State {
	// assume we've consumed '#' '\\' already
	ch := lex.next()
	pk := lex.peek()
	switch {
	case ch == 'a' && pk == 'l': // alarm		
	case ch == 'b' && pk == 'a': // backspace
	case ch == 'd' && pk == 'e': // delete
	case ch == 'e' && pk == 's': // esc
	case ch == 'l' && pk == 'i': // linefeed
		lex.match("inefeed")
		lex.emitDatum(CHAR, SChar('\n'))
	case ch == 'n' && pk == 'e': // newline
		lex.match("ewline")
		lex.emitDatum(CHAR, SChar('\n'))
	case ch == 'v' && pk == 't': // vtab
	case ch == 'p' && pk == 'a': // page
	case ch == 'r' && pk == 'e': // return
		lex.match("eturn")
		lex.emitDatum(CHAR, SChar('\r'))
	case ch == 's' && pk == 'p': // space
		lex.match("pace")
		lex.emitDatum(CHAR, SChar(' '))
	case ch == 't' && pk == 'a': // tab
		lex.match("ab")
		lex.emitDatum(CHAR, SChar('\t'))
	case ch == 'x' && lex.isDigit16():
		lex.peek()
		lex.consume()
		for lex.isDigit16() {
			lex.next()
		}
		lex.backup()
		lex.base = 16
		ret := lex.getInt().(Sint64)
		lex.emitDatum(CHAR, SChar(ret))
	default:
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

	if lex.next() == '.' {
		if lex.next() == '.' {
			lex.emitId("...")
			//lex.emit(ELLIPSIS)
			return (*Lexer).lexToken
		} else {
			panic("expected ...")
		}
	} else if lex.isDigit10() {
		lex.backup()
		lex.backup()
		return (*Lexer).lexNumber
	} else {
		lex.backup()
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
	// #\ character
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

	switch lex.next() {
	case ':':
		lex.emit(KEYWORD)
		return (*Lexer).lexToken
	case '%':
		lex.emit(KSYMBOL)
		return (*Lexer).lexToken
	case '\'':
		lex.emit(SYNTAX)
		return (*Lexer).lexToken
	case '`':
		lex.emit(QSYNTAX)
		return (*Lexer).lexToken
	case ',':
		if lex.peek() == '@' {
			lex.next()
			lex.emit(UNSYNTAXS)
		} else {
			lex.emit(UNSYNTAX)
		}
		return (*Lexer).lexToken
	case '\\':
		return (*Lexer).lexChar
	case '|':
		return (*Lexer).lexNestedComment
	case '!':
		return (*Lexer).lexLineComment
	case ';':
		lex.emit(COMMENT)
		return (*Lexer).lexToken
	case 'E', 'I', 'B', 'O', 'D', 'X':
		fallthrough
	case 'e', 'i', 'b', 'o', 'd', 'x':
		lex.backup()
		lex.backup()
		return (*Lexer).lexNumber
	case 'F':
		fallthrough
	case 'f':
		lex.emitDatum(BOOL, SBool(false))
		return (*Lexer).lexToken
	case 'T':
		fallthrough
	case 't':
		lex.emitDatum(BOOL, SBool(true))
		return (*Lexer).lexToken
	case 'V':
		fallthrough
	case 'v':
		lex.next()
		fallthrough
	case 'U':
		fallthrough
	case 'u':
		lex.match1('8')
		lex.match1('(')
		lex.pcount++
		lex.emit(U8VECTORPAREN)
		return (*Lexer).lexToken
	case '(':
		lex.pcount++
		lex.emit(VECTORPAREN)
		return (*Lexer).lexToken
	}

	if lex.isDigit10() {
		lex.peek()
		lex.consume()
		for lex.isDigit10() {
			lex.next()
		}
		lex.backup()
		lex.base = 10
		label := lex.getInt().(Sint64)
		lex.emitLabel(int(label))
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
	if s != '+' && s != '-' {
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
	case lex.isWhitespace():
		lex.emitId(string([]rune{s}))
		return (*Lexer).lexSpace
	case r == '(':
		fallthrough
	case r == ')':
		fallthrough
	case r == eof:
		lex.emitId(string([]rune{s}))
		return (*Lexer).lexToken
	case r == 'i': // inf.0 or i
		fallthrough
	case r == 'n': // nan.0
		fallthrough
	case lex.isDigit10():
		lex.base = 10
		lex.backup()
		return (*Lexer).lexNumber
	}

	// if this is the end of the token, then it is an id
	//lex.pos += 1
	return (*Lexer).lexId
}

func (lex *Lexer) lexLineComment() State {
	// we have read ';' or
	// we have read '#!'
	for lex.ch != '\n' && lex.ch != eof {
		lex.next()
	}
	lex.backup()
	lex.consume()
	return (*Lexer).lexToken
}

func (lex *Lexer) lexNestedComment() State {
	// we have read '#|'
	// TODO
	for {
		c := lex.ch
		d := lex.next()
		if c == '#' && d == '|' {
			lex.lexNestedComment()
			continue
		}
		if c == '|' && d == '#' {
			lex.consume()
			break
		}
	}
	return (*Lexer).lexToken
}

// <identifier>
func (lex *Lexer) lexId() State {
	for lex.isSubsequent() {
		lex.next()
	}
	lex.backup()
	str := lex.getSpan()
	lex.consume()
	lex.emitId(str)
	return (*Lexer).lexToken
}

// <number>
func (lex *Lexer) lexNumber() State {
	var re Num
	var im Num

	//fmt.Printf("--lexNumber(%s...)--\n", lex.input[lex.start:lex.start+2])
	p := lex.peek()
	if p == '#' {
		lex.next()
		switch lex.next() {
		case 'E':
		case 'e':
			lex.inexact = false
			lex.consume()
			return (*Lexer).lexNumber
		case 'I':
		case 'i':
			lex.inexact = true
			lex.consume()
			return (*Lexer).lexNumber
		case 'B':
		case 'b':
			lex.base = 2
			lex.consume()
			return (*Lexer).lexNumber
		case 'O':
		case 'o':
			lex.base = 8
			lex.consume()
			return (*Lexer).lexNumber
		case 'D':
		case 'd':
			lex.base = 10
			lex.consume()
			return (*Lexer).lexNumber
		case 'X':
		case 'x':
			lex.base = 16
			lex.consume()
			return (*Lexer).lexNumber
		default:
			// this should never happen
			return nil
		}
	}

	lex.acceptSign()
	if lex.isI() {
		lex.next()
		lex.emitNum(NewComplex(NewRational64(0, 1), NewRational64(int64(lex.sign), 1)))
		return (*Lexer).lexToken
	}
	re = lex.getReal()
	switch lex.peek() {
	case '+', '-':
		lex.matchSign()
		if lex.isI() {
			lex.next()
			lex.emitNum(NewComplex(re, NewRational64(int64(lex.sign), 1)))
			return (*Lexer).lexToken
		}
		im = lex.getReal()
		lex.match1('i')
		lex.emitNum(NewComplex(re, im))
	case '@':
		lex.next()
		lex.consume()
		lex.acceptSign()
		im = lex.getReal()
		lex.emitNum(NewComplexPolar(re, im))
	case 'i':
		if lex.isI() {
			lex.next()
			lex.emitNum(NewComplex(NewRational64(0, 1), re))
		}
	default:
		lex.emitNum(re)
	}

	return (*Lexer).lexToken
}

func (lex *Lexer) isI() bool {
	if lex.peek() != 'i' {
		return false
	}
	lex.next()
	if lex.peek() == 'n' {
		lex.backup()
		return false
	}
	lex.backup()
	return true
}

func (lex *Lexer) isSign() bool {
	switch lex.peek() {
	case '+', '-':
		return true
	}
	return false
}

func (lex *Lexer) matchSign() {
	switch lex.peek() {
	case '+':
		lex.sign = 1
		lex.next()
	case '-':
		lex.sign = -1
		lex.next()
	default:
		panic("expected sign")
	}
}

func (lex *Lexer) acceptSign() {
	switch lex.peek() {
	case '-':
		lex.next()
		lex.sign = -1
	case '+':
		lex.next()
		lex.sign = 1
	default:
		lex.sign = 1
	}
}

// <ureal> / <infinity>
// should only be called by lexNumber
func (lex *Lexer) getReal() Num {
	switch lex.peek() {
	case 'i': // must be inf.0
		lex.match("inf.0")
		lex.consume()
		return Sfloat64(math.Inf(lex.sign))
	case 'n': // must be nan.0
		lex.match("nan.0")
		lex.consume()
		return Sfloat64(math.NaN())
	}

	if lex.base != 10 {
		lex.posInt()
		ret := lex.getInt()
		lex.consume()
		return ret
	}

	ret := lex.getDecimal()
	lex.consume()
	return ret
}

// should only be called by lexNumber
func (lex *Lexer) getDecimal() Num {
	//fmt.Printf("--getDecimal(%d)--\n", base)
	if lex.base != 10 {
		panic(newReadError("only decimal fractions supported"))
	}
	if lex.peek() == '.' {
		// form .#
		lex.posFrac()
		return Sfloat64(lex.getFlonum())
	}
	lex.posInt()
	switch lex.peek() {
	case '/':
		// form #/#, only decimal for now
		lex.next()
		lex.posInt()
		return lex.getBigrat()
	case '.':
		// form #.#
		lex.posFrac()
		return Sfloat64(lex.getFlonum())
	case 'e':
		// form #e#
		return Sfloat64(lex.getFlonum())
	}
	// form #
	return lex.getInt()
}

// should only be called by lexNumber
func (lex *Lexer) getInt() Num {
	if lex.pos-lex.start > 18 {
		return lex.getBigint()
	}
	return Sint64(lex.getFixnum())
}

// should only be called after posInt, posFrac
func (lex *Lexer) getFlonum() float64 {
	lex.getSuffix()
	str := lex.getSpan()
	lex.consume()
	num, err := strconv.ParseFloat(str, 64)
	if err != nil {
		panic(err)
	}
	return num
}

// should only be called after posInt
func (lex *Lexer) getFixnum() int64 {
	str := lex.getSpan()
	lex.consume()
	//fmt.Printf("getFixnum(%s)\n", str)
	num, err := strconv.ParseInt(str, lex.base, 64)
	if err != nil {
		panic(err)
	}
	return num
}

func (lex *Lexer) getBigint() SInteger {
	//fmt.Printf("--getBigint(%s)--\n", lex.getSpan())
	str := lex.getSpan()
	lex.consume()
	num, err := big.NewInt(0).SetString(str, lex.base)
	if num == nil {
		panic(err)
	}
	return SInteger{it: num}
}

func (lex *Lexer) getBigrat() SRational {
	//fmt.Printf("--getBigrat(%s)--\n", lex.getSpan())
	str := lex.getSpan()
	lex.consume()
	num, err := big.NewRat(0, 1).SetString(str)
	if num == nil {
		panic(err)
	}
	return SRational{it: num}
}

func (lex *Lexer) getSuffix() (exp int, prec int) {
	//fmt.Printf("--getSuffix(%s)--\n", lex.getSpan())
	var expSign int
	switch lex.peek() {
	case 'e':
	case 's':
		prec = 10
	case 'f':
		prec = 23
	case 'd':
		prec = 52
	case 'l':
		prec = 112
	default:
		return 0, -1
	}
	lex.next()
	switch lex.peek() {
	case '+':
		lex.next()
		expSign = 1
	case '-':
		lex.next()
		expSign = -1
	}
	lex.posInt()
	str := lex.getSpan()
	lex.consume()
	expAbs, err := strconv.ParseInt(str, lex.base, 64)
	if err != nil {
		panic(err)
	}
	return expSign * int(expAbs), prec
}

// <uinteger>
// should only be called by lexNumber
func (lex *Lexer) posInt() {
	lex.peek()
	switch lex.base {
	case 2:
		for lex.isDigit2() {
			lex.next()
		}
	case 8:
		for lex.isDigit8() {
			lex.next()
		}
	case 10:
		for lex.isDigit10() {
			lex.next()
		}
	case 16:
		for lex.isDigit16() {
			lex.next()
		}
	}
	lex.backup()
}

// should only be called by lexNumber
func (lex *Lexer) posFrac() {
	lex.match1('.')
	lex.peek()
	for lex.isDigit10() {
		lex.next()
	}
	lex.backup()
}

// character class methods

func (lex *Lexer) isLetter() bool {
	//fmt.Printf("\n-- isLetter() --\n")
	return 'a' <= lex.ch && lex.ch <= 'z' || 'A' <= lex.ch && lex.ch <= 'Z'
}

func (lex *Lexer) isDigit2() bool {
	return '0' == lex.ch || lex.ch == '1'
}

func (lex *Lexer) isDigit8() bool {
	return '0' <= lex.ch && lex.ch <= '7'
}

func (lex *Lexer) isDigit10() bool {
	return '0' <= lex.ch && lex.ch <= '9'
}

func (lex *Lexer) isDigit16() bool {
	if lex.isDigit10() {
		return true
	}
	if 'a' <= lex.ch && lex.ch <= 'f' {
		return true
	}
	if 'A' <= lex.ch && lex.ch <= 'F' {
		return true
	}
	return false
}

func (lex *Lexer) isInitial() bool {
	//fmt.Printf("\n-- isInitial() --\n")
	return lex.isLetter() || lex.isSpecialInitial()
}

func (lex *Lexer) isSubsequent() bool {
	return lex.isInitial() || lex.isDigit10() || lex.isSpecialSubsequent()
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
	if lex.ch == ' ' || lex.ch == '\t' || lex.ch == '\n' || lex.ch == '\r' {
		return true
	}
	return false
}

// ⟨string element⟩ -> ⟨any character other than " or \⟩ | \" | \\
func (lex *Lexer) isStringElement() bool {
	if lex.ch == '"' {
		return false
	}
	return true
}
