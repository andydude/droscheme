// -*- mode: go -*-
//
// Droscheme - a Scheme implementation
//
// This file is a parser description for the Scheme language
// as understood by Droscheme. In particular, we recognize
// multiple revisions of the standard, most notably:
//
//  - R6RS bytevectors #vu8(...)
//  - R7RS/SRFI-4 bytevectors #u8(...)
//  - R6RS numbers (+inf.0, -inf.0, ...|23)
//  - R7RS numbers (subset of R6RS numbers)
//
	
// === Implementation Details:
// 
// There are two fundamental types that are used to pass
// information between the lexer, the parser, and the rest
// of the program.
//
//  - yySymType - every production has access to this type, but
//                is it only visible inside this file?
//
//  - Lexer     - this is the only thing that is available
//                from outside this file, so we have an
//                lval field in here to communicate.
//


// BEGIN includes
%{
package droscheme

import (
	"fmt"
	"strconv"
//	"utf8"
)

%}
// END includes

// expands to yySymType
%union
{
    datum Any;
    label int;
}
// BEGIN tokens

%token <datum> Id
%token <datum> Bool
%token <datum> Number
%token <datum> Char
%token <datum> String
%token <label> Label

//%left Datums1
//%left Datums0

%start Datum

// END tokens
%%
// BEGIN grammar

// R7RS 7.1.2. External representations

Datum:
	SimpleDatum
	{
        //a SimpleDatum
	//$$ = $1
	}
|	CompoundDatum
	{
        //a CompoundDatum
	//$$ = $1
	}
|	Label '=' Datum
	{
        //a LabeledDatum
	    // set Env.label[n] = datum
	}
|	Label '#'
	{
        //a DatumLabel
	    // get Env.label[n]
	}

Datums1:
	Datum
	{
        //an single Datums1
	}
|	Datums1 Datum
	{
        //an multiple Datums1
	}
// TBD: should we use (Datum Datums1) instead?

Datums0:
	Datums1
	{
        //an nonempty Datums0
	}
|	/*empty*/
	{
        //an empty Datums0
	}

SimpleDatum:
	Bool
	{
        //a Bool
	}
|	Number
	{
        //a Number
	}
|	Char
	{
        //a Char
	}
|	String
	{
        //a String
	}
|	Symbol
	{
        //a Symbol
	}
// This is our name for bytevector, in case we want to support all SRFI-4 vectors
|	U8Vector
	{
        //a U8Vector
	}

Symbol:
	Id
	{
        //an Identifier
	}

CompoundDatum:
	List
	{
        //a List
	}
|	Vector
	{
        //a Vector
	}

List:
	'(' Datums0 ')'
	{
        //a proper List
	}
|	'(' Datums1 '.' Datum ')'
	{
        //an improper List (dotted)
	}
|	Abbreviation

Abbreviation:
	'\'' Datum
	{
        //a (quote)
	}
|	'`' Datum
	{
        //a (quasiquote)
	}
|	',@' Datum
	{
        //an (unquote-splicing)
	}
|	',' Datum
	{
        //an (unquote)
	}

Vector:
	VectorParen Datums1 ')'
	{
        //a Vector literal
	}

U8Vector:
	U8VectorParen U8Num ')'
	{
        //a U8Vector literal
	}

VectorParen:
	'#('

U8VectorParen:
	'#u8('
|	'#vu8('

U8Num:
	Number
//	Digit
//|	Digit Digit
//|	Digit Digit Digit

//Label:
//	'#' Digits1

// END grammar
%%
// BEGIN lexer

//type Lexer struct {
//    port chan Rune
//    peek rune
//	lval *yySymType
//}

// This should probably be int
//type Rune byte

// This is from units.y example in goyacc source
//func getrune() int {
//    var c, n int
//
//    if linep >= len(line) {
//        return 0
//    }
//    c, n = utf8.DecodeRuneInString(line[linep:len(line)])
//    linep += n
//    if c == '\n' {
//        c = 0
//    }
//    return c
//}

//func (l Lexer) GetRune() Rune {
//	l.peek = <-l.port;
//	return l.peek;
//}

// func (l Lexer) Lex(lval *yySymType) int {
//     c := l.GetRune()

// 	if '0' <= c && c <= '9' {
//         // TODO lex rest of number
// 		return Number
// 	}

// 	if 'a' <= c && c <= 'z' {
//         // TODO lex rest of identifier
// 		return Id
// 	}

//     // TODO lex other things
// 	return (-1)
// }

// func (l Lexer) Error(s string) {
// }

// END lexer

// func toPort(s string, port chan Rune) {
// 	// probably too many runes
// 	//r := NewString(s)
// 	//l := r.RuneCount()
// 	//for var i int := 0; i < l; i++ {
// 	//	port <- r.At(i)
// 	//}
// }

// func ReadDatumFromString(s string) Any {
// 	port := make(chan Rune, len(s))
// 	toPort(s, port)
// 	return ReadDatum(port)
// }

// func ReadDatum(port chan Rune) Any {
//     lex := Lexer{port, 0, nil}
//     yyParse(lex)
// 	return lex.lval.datum
// }

type SimpleLexer struct {
	input string
	pos int
	ch byte
}

func (lex *SimpleLexer) consume() {
	lex.pos++
	lex.ch = lex.input[lex.pos]
}

func (lex *SimpleLexer) match(ch byte) {
	if ch != lex.ch {
		panic("failed to match")
	}
	lex.consume()
}

func (lex *SimpleLexer) eat() byte {
	ch := lex.ch
	lex.consume()
	return ch
}

func (lex *SimpleLexer) isLetter() bool {
	return 'a' <= lex.ch && lex.ch <= 'z' || 'A' <= lex.ch && lex.ch <= 'Z'
}

func (lex *SimpleLexer) isNumber() bool {
	return '0' <= lex.ch && lex.ch <= '9'
}

func (lex *SimpleLexer) isIdInitial() bool {
	return lex.isLetter() || lex.isIdSpecialInitial()
}

func (lex *SimpleLexer) isIdSpecialInitial() bool {
	switch lex.ch {
	case '!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '~', '_', '^':
		return true
	}
	return false
}

func (lex *SimpleLexer) isIdSpecialSubsequent() bool {
	return lex.ch == '.' || lex.ch == '+' || lex.ch == '-'
}

func (lex *SimpleLexer) readId() Any {
	id := []byte{lex.eat()}
	for lex.isIdInitial() || lex.isNumber() || lex.isIdSpecialSubsequent() {
		id = append(id, lex.ch)
	}
	return SSymbol{string(id)}
}

func (lex *SimpleLexer) readNumber() Any {
	num := []byte{lex.eat()}
	for lex.isNumber() {
		num = append(num, lex.ch)
	}
	x, err := strconv.ParseInt(string(num), 10, 64)
	if err != nil {
		panic("Invalid number")
	}
	return Sint64(x)
}

func (lex *SimpleLexer) readBool() Any {
	if lex.ch == 't' {
		lex.consume()
		return SBool(true)
	} else if lex.ch == 'f' {
		lex.consume()
		return SBool(false)
	}
	panic("Unknown boolean")
}

func (lex *SimpleLexer) Lex(lval *yySymType) int {
	for lex.pos < len(lex.input) {
		switch {
		case lex.ch == ')':
			return ')'
		case lex.ch == '(':
			return '('
		case lex.ch == '\'':
			return '\''
		case lex.ch == '#':
			lex.consume()
			if lex.ch == 't' || lex.ch == 'f' {
				lval.datum = lex.readBool()
				lval.label = Bool
				return Bool
			}
			return '#'
		case lex.ch == '+' || lex.ch == '-':
			lval.datum = lex.readId()
			lval.label = Id
			return Id
		case lex.ch == '.':
			lex.match('.')
			lex.match('.')
			lex.match('.')
			lval.datum = SSymbol{"..."}
			lval.label = Id
			return Id
		case lex.isIdInitial():
			lval.datum = lex.readId()
			lval.label = Id
			return Id
		case lex.isNumber():
			lval.datum = lex.readNumber()
			lval.label = Number
			return Number
		}
	}
	return int(lex.ch)
}

func (lex *SimpleLexer) Error(e string) {
	panic(e)
}

func Read(input string) Any {
	lex := &SimpleLexer{input, 0, input[0]}
	yyParse(lex)
	return nil
}
