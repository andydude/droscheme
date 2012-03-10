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
	"strings"
)

var ret Any // returned value from yyParse
var err error // return value for parsing errors

%}
// END includes

// expands to yySymType
%union {
    datum Any;
    label int; // this is for recursive structures
	token int; // this is for token identifiers
}

%type <datum> datum datums1 datums0 simpledatum compounddatum
%type <datum> symbol list vector u8vector abbreviation

// BEGIN tokens

%token <datum> ID
%token <datum> BOOL
%token <datum> NUMBER
%token <datum> CHAR
%token <datum> STRING
%token <label> LABEL
%token <token> VECTORPAREN
%token <token> U8VECTORPAREN

//%left Datums1
//%left Datums0

%start datum

// END tokens
%%
// BEGIN grammar

// R7RS 7.1.2. External representations

datum:
	simpledatum
	{
	$$ = $1
	ret = $$
	}
|	compounddatum
	{
	$$ = $1
	ret = $$
	}
|	LABEL '=' datum
	{
	//$$.label = $1
	//$$.datum = $3
	}
|	LABEL '#'
	{
	//$$.label = $1
	}

simpledatum:
	BOOL
	{
	// a BOOL
	$$ = $1
	}
|	NUMBER
	{
    // a NUMBER
	$$ = $1
	}
|	CHAR
	{
	$$ = $1
        //a CHAR
	}
|	STRING
	{
	$$ = $1
        //a STRING
	}
|	symbol
	{
	$$ = $1
	// a symbol
	}
// This is our name for bytevector, in case we want to support all SRFI-4 vectors
|	u8vector
	{
	$$ = $1
        //a u8vector
	}

symbol:
	ID
	{
	$$ = $1
	}

compounddatum:
	list
	{
	// a list
    $$ = $1
	}
|	vector
	{
    $$ = $1
	}

list:
	'(' datums0 ')'
	{
    $$ = $2
	}
|	'(' datums1 '.' datum ')'
	{
    //an improper list (dotted)
	$$ = listR($2, $4)
	}
|	abbreviation
    {
	$$ = $1
    }

datums1:
	datum
	{
	$$ = SPair{$1, SNull{}}
	}
|	datum datums1 
	{
	$$ = SPair{$1, $2}
	}

datums0:
	datums1
	{
	$$ = $1
	}
|	/*empty*/
	{
	$$ = SNull{}
	}

abbreviation:
	'\'' datum
	{
        //a (quote)
	}
|	'`' datum
	{
        //a (quasiquote)
	}
|	',@' datum
	{
        //an (unquote-splicing)
	}
|	',' datum
	{
        //an (unquote)
	}

vector:
	VECTORPAREN datums0 ')'
	{
	$$ = DlistZKZRvector(list1($2))
	}

u8vector:
	U8VECTORPAREN datums0 ')'
	{
	$$ = Du8ZKlistZKZRbytevector(list1($2))
	}

//LABEL:
//	'#' Digits1

// END grammar
%%
// BEGIN lexer

type Lexer struct {
	input string
	pos int
	ch rune
}

func (lex *Lexer) consume() {
	lex.pos++
	if lex.pos < len(lex.input) {
		lex.ch = rune(lex.input[lex.pos])
	} else {
		lex.ch = -1
	}
}

func (lex *Lexer) match(ch rune) {
	if ch != lex.ch {
		panic("failed to match")
	}
	lex.consume()
}

func (lex *Lexer) eat() rune {
	ch := lex.ch
	lex.consume()
	return ch
}

func (lex *Lexer) isLetter() bool {
	return 'a' <= lex.ch && lex.ch <= 'z' || 'A' <= lex.ch && lex.ch <= 'Z'
}

func (lex *Lexer) isNUMBER() bool {
	return '0' <= lex.ch && lex.ch <= '9'
}

func (lex *Lexer) isIDInitial() bool {
	return lex.isLetter() || lex.isIDSpecialInitial()
}

func (lex *Lexer) isIDSpecialInitial() bool {
	switch lex.ch {
	case '!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '~', '_', '^':
		return true
	}
	return false
}

func (lex *Lexer) isIDSpecialSubsequent() bool {
	return lex.ch == '.' || lex.ch == '+' || lex.ch == '-'
}

func (lex *Lexer) readID() Any {
	id := []rune{lex.eat()}
	for lex.isIDInitial() || lex.isNUMBER() || lex.isIDSpecialSubsequent() {
		id = append(id, lex.eat())
	}
	return SSymbol{string(id)}
}

func (lex *Lexer) readNUMBER() Any {
	num := []rune{lex.eat()}
	for lex.isNUMBER() {
		num = append(num, lex.eat())
	}
	x, err := strconv.ParseInt(string(num), 10, 64)
	if err != nil {
		panic("Invalid number")
	}
	return Sint64(x)
}

func (lex *Lexer) readBOOL() Any {
	if lex.ch == 't' {
		lex.consume()
		return SBool(true)
	} else if lex.ch == 'f' {
		lex.consume()
		return SBool(false)
	}
	panic("Unknown boolean")
}

func (lex *Lexer) readU8VECTORPAREN() int {
	if lex.ch == 'v' {
		lex.match('v')
	}
	lex.match('u')
	lex.match('8')
	lex.match('(')
	return U8VECTORPAREN
}

func (lex *Lexer) isWhitespace() bool {
	if lex.ch == ' ' || lex.ch == '\t' || lex.ch == '\n' || lex.ch == 'r' {
		return true
	}
	return false
}

func (lex *Lexer) Lex(lval *yySymType) int {
	for lex.pos < len(lex.input) {
		switch {
		case lex.isWhitespace():
			lex.consume()
		case lex.ch == ')':
			lex.consume()
			return ')'
		case lex.ch == '(':
			lex.consume()
			return '('
		case lex.ch == '\'':
			lex.consume()
			return '\''
		case lex.ch == '#':
			lex.consume()
			if lex.ch == 't' || lex.ch == 'f' {
				lval.datum = lex.readBOOL()
				lval.token = BOOL
				return BOOL
			}
			if lex.ch == 'u' || lex.ch == 'v' {
				lval.token = lex.readU8VECTORPAREN()
				return U8VECTORPAREN
			}
			if lex.ch == '(' {
				lex.consume()
				return VECTORPAREN
			}
			return '#'
		case lex.ch == '.':
			lex.consume()
			if lex.ch == '.' {
				lex.match('.')
				lex.match('.')
				lval.datum = SSymbol{"..."}
				lval.token = ID
				return ID
			} else {
				return '.'
			}
		case lex.isIDInitial():
			lval.datum = lex.readID()
			lval.token = ID
			return ID
		case lex.ch == '+' || lex.ch == '-':
			lex.consume()
			if lex.isWhitespace() {
				// if this is the end of the token, then it is an id
				lval.datum = lex.readID()
				lval.token = ID
				return ID
			}
			// if this is the beginning of the token, then it is a number
			fallthrough
		case lex.isNUMBER():
			lval.datum = lex.readNUMBER()
			lval.token = NUMBER
			return NUMBER
		default:
			lex.consume()
			return int(lex.ch)
		}
	}
	return -1
}

func (lex *Lexer) Error(e string) {
	err = fmt.Errorf("Syntax error at position %d in line %s: %s", lex.pos, lex.input, e)
}

func Read(input string) (Any, error) {
	input = strings.TrimSpace(input)
	if input == "" {
		return nil, nil
	}
	lex := &Lexer{input, 0, rune(input[0])}
	yyParse(lex)
	err2 := err
	err = nil
	return ret, err2
}
