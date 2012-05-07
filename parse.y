/*
 * Droscheme - a Scheme implementation
 * Copyright © 2012 Andrew Robbins, Daniel Connelly
 *
 * This program is free software: it is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
 */
/* This file is a parser description for the Scheme language
 * as understood by Droscheme. In particular, we recognize
 * multiple revisions of the standard, most notably:
 *
 *  - R6RS bytevectors #vu8(...)
 *  - R7RS/SRFI-4 bytevectors #u8(...)
 *  - R6RS numbers (+inf.0, -inf.0, ...|23)
 *  - R7RS numbers (subset of R6RS numbers)
 */
/* Implementation Details:
 *
 * There are two fundamental types that are used to pass
 * information between the lexer, the parser, and the rest
 * of the program.
 *
 *  - yySymType - every production has access to this type, but
 *                is it only visible inside this file?
 *
 *  - Lexer     - this is the only thing that is available
 *                from outside this file, so we have an
 *                lval field in here to communicate.
 */
%{
package droscheme

import (
	"fmt"
)

%}

// expands to yySymType
%union {
    datum Any;
	token int; // this is for token identifiers
}

%type <datum> datum datums2 datums1 datums0 simpledatum compounddatum
%type <datum> keyword symbol list vector u8vector abbreviation

// BEGIN tokens

%token <datum> ID
%token <datum> BOOL
%token <datum> NUMBER
%token <datum> CHAR
%token <datum> STRING
%token <datum> LABEL
%token <token> VECTORPAREN   /* "#(" */
%token <token> U8VECTORPAREN /* "#u8(" */
%token <token> QUOTE     /* "'" */
%token <token> QQUOTE    /* "`" */
%token <token> UNQUOTE   /* "," */
%token <token> UNQUOTES  /* ",@" */
%token <token> SYNTAX    /* "#'" */
%token <token> QSYNTAX   /* "#`" */
%token <token> UNSYNTAX  /* "#," */
%token <token> UNSYNTAXS /* "#,@" */
%token <token> ELLIPSIS  /* "..." */
%token <token> KSYMBOL   /* "#%" */
%token <token> KEYWORD   /* "#:" */
%token <token> COMMENT   /* "#;" */

%start datum

// END tokens
%%
// BEGIN grammar

// R7RS 7.1.2. External representations

datum:
	simpledatum
	{
		$$ = $1
        yylex.(*Lexer).value = $$
	}
|	compounddatum
	{
		$$ = $1
        yylex.(*Lexer).value = $$
	}
|   KSYMBOL datum
	{
		$$ = NewSymbol("#%" + $2.(SSymbol).name)
        yylex.(*Lexer).value = $$
	}
|	keyword datum
	{
		$$ = NewKeyword($1, $2)
        yylex.(*Lexer).value = $$
	}
|	comment datum
	{
		$$ = $2
        yylex.(*Lexer).value = $$
	}
|	LABEL '=' datum
	{
        $$ = NewLabel($1, $3)
        yylex.(*Lexer).value = $$
	}
|	LABEL '#'
	{
		$$ = $1
        yylex.(*Lexer).value = $$
	}

keyword:
	KEYWORD datum
	{
        $$ = $2
	}
comment:
	COMMENT datum
	{
	}

symbol:
	ID
	{
        $$ = $1
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
|	u8vector
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
|	'[' datums0 ']'
	{
        $$ = $2
	}
|	'(' datums2 ')'
	{
        $$ = $2
	}
|	'[' datums2 ']'
	{
        $$ = $2
	}
|	abbreviation
    {
        $$ = $1
    }

datums2:
	datums1 '.' datum
	{
        $$ = listR($1, $3)
	}
|	datums1 '.' datum '.' datum
	{
        if (ToFixnum(Dlength(list1($1))) != 1) {
            panic("double-dotted-lists must have 3 elements")
        }
        $$ = list3($3, unlist1($1), $5)
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
|	datum comment
	{
        $$ = list1($1)
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
        $$ = Dvector($2)
	}

u8vector:
	U8VECTORPAREN datums0 ')'
	{
        $$ = DbytevectorZKu8($2)
	}

// END grammar
%%
// BEGIN functions

func (lex *Lexer) Lex(lval *yySymType) int {
	tok := lex.nextToken()
	lval.datum = tok.datum
	lval.token = tok.token
	return lval.token
}

func (lex *Lexer) Error(e string) {
	lex.err = fmt.Errorf("Syntax error at position %d in line %s: %s", lex.pos, lex.input, e)
}

// END functions
