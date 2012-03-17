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
%token <token> VECTORPAREN   /* "#(" */
%token <token> U8VECTORPAREN /* "#u8(" */
%token <token> QUOTE     /* "'" */
%token <token> QQUOTE    /* "`" */
%token <token> UNQUOTE   /* "," */
%token <token> UNQUOTES  /* ",@" */
%token <token> SYNTAX	 /* "#'" */
%token <token> QSYNTAX	 /* "#`" */
%token <token> UNSYNTAX  /* "#," */
%token <token> UNSYNTAXS /* "#,@" */
%token <token> ELIPSIS   /* "..." */

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
	$$ = $1
	}
|	NUMBER
	{
	$$ = $1
	}
|	CHAR
	{
	$$ = $1
	}
|	STRING
	{
	$$ = $1
	}
|	symbol
	{
	$$ = $1
	}
// This is our name for bytevector, in case we want to support all SRFI-4 vectors
|	u8vector
	{
	$$ = $1
	}

symbol:
	ID
	{
	$$ = $1
	}

compounddatum:
	list
	{
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
	$$ = listR($2, $4)
	}
|	abbreviation
    {
	$$ = $1
    }

datums1:
	datum
	{
	$$ = list1($1)
	}
|	datum datums1 
	{
	$$ = list1R($1, $2)
	}

datums0:
	datums1
	{
	$$ = $1
	}
|	/*empty*/
	{
	$$ = list0()
	}

abbreviation:
	QUOTE datum
	{
	$$ = list2(SSymbol{"quote"}, $2)
	}
|	QQUOTE datum
	{
	$$ = list2(SSymbol{"quasiquote"}, $2)
	}
|	UNQUOTE datum
	{
	$$ = list2(SSymbol{"unquote"}, $2)
	}
|	UNQUOTES datum
	{
	$$ = list2(SSymbol{"unquote-splicing"}, $2)
	}
|	SYNTAX datum
	{
	$$ = list2(SSymbol{"syntax"}, $2)
	}
|	QSYNTAX datum
	{
	$$ = list2(SSymbol{"quasisyntax"}, $2)
	}
|	UNSYNTAX datum
	{
	$$ = list2(SSymbol{"unsyntax"}, $2)
	}
|	UNSYNTAXS datum
	{
	$$ = list2(SSymbol{"unsyntax-splicing"}, $2)
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

func (lex *Lexer) Lex(lval *yySymType) int {
	tok := lex.nextToken()
	// can we use copy() instead?
	lval.datum = tok.datum
	lval.token = tok.token
	return lval.token
}

func (lex *Lexer) Error(e string) {
	err = fmt.Errorf("Syntax error at position %d in line %s: %s", lex.pos, lex.input, e)
}

func Read(input string) (Any, error) {
	input = strings.TrimSpace(input)
	if input == "" {
		return nil, nil
	}
	lex := newLexer(input)
	yyParse(lex)
	err2 := err
	err = nil
	return ret, err2
}
