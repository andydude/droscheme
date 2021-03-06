/*
 * Droscheme - a Scheme implementation
 * Copyright © 2012 Andrew Robbins, Daniel Connelly
 *
 * This program is free software: it is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
 */
package droscheme

import (
	"fmt"
	"math"
	"math/big"
	"strconv"
	"strings"
	"unicode/utf8"
)

const eof = 0x7F

var sign int = 1
var base int = 10
var inexact bool = false

// [AJR] Yes, I watched Rob Pike
type State func(*Lexer) State
type Lexer struct {
	input             string
	start, pos, width int
	tokens            chan *yySymType
	state             State
	ch                rune
}

func newLexer(input string) *Lexer {
	const bufferSize = 5
	lex := &Lexer{
		input:  input,
		tokens: make(chan *yySymType, bufferSize),
		state:  (*Lexer).lexToken,
		ch:     rune(input[0]),
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

func (lex *Lexer) emitLabel(label int) {
	lex.emitDatum(LABEL, NewLabel(Sint64(label), nil))
}

func (lex *Lexer) emitDatum(token int, datum Any) {
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
	lex.pos -= lex.width
}

func (lex *Lexer) consume() {
	lex.start = lex.pos
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
	return lex.input[lex.start:lex.pos]
}

func (lex *Lexer) nextToken() *yySymType {
	for {
		select {
		case tok := <-lex.tokens:
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
	lex.peek()
	lex.pos += lex.width
	return lex.ch
}

func (lex *Lexer) peek() rune {
	if lex.pos >= len(lex.input) {
		lex.ch, lex.width = eof, 0
	} else {
		lex.ch, lex.width = utf8.DecodeRuneInString(lex.input[lex.pos:])
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
	lex.start = lex.pos
	base = 10
	sign = 1
	return (*Lexer).lexToken
}

func (lex *Lexer) lexToken() State {
	switch r := lex.peek(); {
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
		if lex.ch == '\\' {
			lex.next()
			switch lex.ch {
			case 'a': lex.ch = 0x07
			case 'b': lex.ch = 0x08
			case 't': lex.ch = 0x09
			case 'n': lex.ch = 0x0A
			case 'v': lex.ch = 0x0B
			case 'f': lex.ch = 0x0C
			case 'r': lex.ch = 0x0D
			case 'x':
				lex.peek()
				lex.consume()
				for lex.isDigit16() {
					lex.next()
				}
				lex.backup()
				base = 16
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
		base = 16
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
		return (*Lexer).lexId
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
		lex.emit(U8VECTORPAREN)
		return (*Lexer).lexToken
	case '(':
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
		base = 10
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
	case r == 'i': // inf.0 or i
		fallthrough
	case r == 'n': // nan.0
		fallthrough
	case lex.isDigit10():
		base = 10
		lex.backup()
		return (*Lexer).lexNumber
	}

	// if this is the end of the token, then it is an id
	lex.pos += 1
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
	str := lex.input[lex.start:lex.pos]
	//fmt.Printf("\n-- lexId() %s--\n", str)
	lex.emitId(str)
	return (*Lexer).lexToken
}

// <number>
func (lex *Lexer) lexNumber() State {
	var re Num
	var im Num

	//fmt.Printf("--lexNumber(%s...)--\n", lex.input[lex.start:lex.start+2])

	if lex.next() == '#' {
		switch lex.next() {
		case 'E':
		case 'e':
			inexact = false
			lex.consume()
			return (*Lexer).lexNumber
		case 'I':
		case 'i':
			inexact = true
			lex.consume()
			return (*Lexer).lexNumber
		case 'B':
		case 'b':
			base = 2
			lex.consume()
			return (*Lexer).lexNumber
		case 'O':
		case 'o':
			base = 8
			lex.consume()
			return (*Lexer).lexNumber
		case 'D':
		case 'd':
			base = 10
			lex.consume()
			return (*Lexer).lexNumber
		case 'X':
		case 'x':
			base = 16
			lex.consume()
			return (*Lexer).lexNumber
		default:
			// this should never happen
			return nil
		}
	} else {
		lex.backup()
	}

	lex.acceptSign()
	if lex.isI() {
		lex.next()
		lex.emitNum(NewComplex(NewRational64(0, 1), NewRational64(int64(sign), 1)))
		return (*Lexer).lexToken
	}
	re = lex.getReal()
	switch lex.peek() {
	case '+', '-':
		lex.matchSign()
		if lex.isI() {
			lex.next()
			lex.emitNum(NewComplex(re, NewRational64(int64(sign), 1)))
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
		sign = 1
		lex.next()
	case '-':
		sign = -1
		lex.next()
	default:
		panic("expected sign")
	}
}

func (lex *Lexer) acceptSign() {
	switch lex.peek() {
	case '-':
		lex.next()
		sign = -1
	case '+':
		lex.next()
		sign = 1
	default:
		sign = 1
	}
}

// <ureal> / <infinity>
// should only be called by lexNumber
func (lex *Lexer) getReal() Num {
	//fmt.Printf("--getReal(%d)--\n", base)
	switch lex.peek() {
	case 'i': // must be inf.0
		lex.match("inf.0")
		lex.consume()
		return Sfloat64(math.Inf(sign))
	case 'n': // must be nan.0
		lex.match("nan.0")
		lex.consume()
		return Sfloat64(math.NaN())
	}

	if base != 10 {
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
	if base != 10 {
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
	num, err := strconv.ParseInt(str, base, 64)
	if err != nil {
		panic(err)
	}
	return num
}

func (lex *Lexer) getBigint() SInteger {
	//fmt.Printf("--getBigint(%s)--\n", lex.getSpan())
	str := lex.getSpan()
	lex.consume()
	num, err := big.NewInt(0).SetString(str, base)
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
	//fmt.Printf("--getSuffix(%s)--\n", lex.getSpan())
	cur := lex.pos
	switch lex.peek() {
	case '+':
		lex.next()
		expSign = 1
	case '-':
		lex.next()
		expSign = -1
	}
	//fmt.Printf("--getSuffix(%s)--b\n", lex.getSpan())
	lex.posInt()
	//fmt.Printf("--getSuffix(%s)--a\n", lex.getSpan())
	expAbs, err := strconv.ParseInt(lex.input[cur:lex.pos], base, 64)
	if err != nil {
		panic(err)
	}
	return expSign * int(expAbs), prec
}

// <uinteger>
// should only be called by lexNumber
func (lex *Lexer) posInt() {
	lex.peek()
	switch base {
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
