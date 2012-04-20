
//line parse.y:33
package droscheme

import (
	"fmt"
	"strings"
)

var parseValue Any // returned value from yyParse
var parseErr error // return value for parsing errors


//line parse.y:46
type yySymType struct {
	yys int
    datum Any;
	token int; // this is for token identifiers
}

const ID = 57346
const BOOL = 57347
const NUMBER = 57348
const CHAR = 57349
const STRING = 57350
const LABEL = 57351
const VECTORPAREN = 57352
const U8VECTORPAREN = 57353
const QUOTE = 57354
const QQUOTE = 57355
const UNQUOTE = 57356
const UNQUOTES = 57357
const SYNTAX = 57358
const QSYNTAX = 57359
const UNSYNTAX = 57360
const UNSYNTAXS = 57361
const ELLIPSIS = 57362

var yyToknames = []string{
	"ID",
	"BOOL",
	"NUMBER",
	"CHAR",
	"STRING",
	"LABEL",
	"VECTORPAREN",
	"U8VECTORPAREN",
	"QUOTE",
	"QQUOTE",
	"UNQUOTE",
	"UNQUOTES",
	"SYNTAX",
	"QSYNTAX",
	"UNSYNTAX",
	"UNSYNTAXS",
	"ELLIPSIS",
}
var yyStatenames = []string{}

const yyEofCode = 1
const yyErrCode = 2
const yyMaxDepth = 200

//line parse.y:234

// BEGIN functions

func (lex *Lexer) Lex(lval *yySymType) int {
	tok := lex.nextToken()
	lval.datum = tok.datum
	lval.token = tok.token
	return lval.token
}

func (lex *Lexer) Error(e string) {
	parseErr = fmt.Errorf("Syntax error at position %d in line %s: %s", lex.pos, lex.input, e)
}

func Read(input string) (Any, error) {
	input = strings.TrimSpace(input)
	if input == "" {
		return nil, nil
	}
	lex := newLexer(input)
	yyParse(lex)
	err := parseErr
	parseErr = nil
	return parseValue, err
}

// END functions

//line yacctab:1
var yyExca = []int{
	-1, 1,
	1, -1,
	-2, 0,
}

const yyNprod = 33
const yyPrivate = 57344

var yyTokenNames []string
var yyStates []string

const yyLast = 79

var yyAct = []int{

	31, 1, 51, 49, 56, 50, 55, 52, 48, 46,
	27, 28, 17, 10, 12, 11, 9, 3, 2, 0,
	37, 38, 39, 40, 41, 42, 43, 44, 45, 0,
	13, 5, 6, 7, 8, 4, 18, 14, 19, 20,
	21, 22, 23, 24, 25, 26, 30, 0, 29, 15,
	53, 16, 54, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 33, 35, 32, 34, 0, 36, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 47,
}
var yyPact = []int{

	26, -1000, -1000, -1000, -11, -1000, -1000, -1000, -1000, -1000,
	-1000, -1000, -1000, -1000, 26, 26, 26, -1000, 26, 26,
	26, 26, 26, 26, 26, 26, 26, 26, -1000, -15,
	-1000, 26, -16, -24, -21, -25, -17, -1000, -1000, -1000,
	-1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, 26,
	-1000, 26, -1000, -18, -22, -1000, -1000,
}
var yyPgo = []int{

	0, 0, 46, 48, 18, 17, 16, 15, 14, 13,
	12,
}
var yyR1 = []int{

	0, 1, 1, 1, 1, 4, 4, 4, 4, 4,
	4, 6, 5, 5, 7, 7, 7, 7, 7, 2,
	2, 3, 3, 10, 10, 10, 10, 10, 10, 10,
	10, 8, 9,
}
var yyR2 = []int{

	0, 1, 1, 3, 2, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 3, 3, 5, 5, 1, 1,
	2, 1, 0, 2, 2, 2, 2, 2, 2, 2,
	2, 3, 3,
}
var yyChk = []int{

	-1000, -1, -4, -5, 9, 5, 6, 7, 8, -6,
	-9, -7, -8, 4, 11, 23, 25, -10, 10, 12,
	13, 14, 15, 16, 17, 18, 19, 21, 22, -3,
	-2, -1, -3, -2, -3, -2, -3, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, 24, -2, 24, 27,
	26, 27, 24, -1, -1, 24, 26,
}
var yyDef = []int{

	0, -2, 1, 2, 0, 5, 6, 7, 8, 9,
	10, 12, 13, 11, 22, 22, 22, 18, 22, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 4, 0,
	21, 19, 0, 21, 0, 21, 0, 23, 24, 25,
	26, 27, 28, 29, 30, 3, 32, 20, 14, 0,
	15, 0, 31, 0, 0, 16, 17,
}
var yyTok1 = []int{

	1, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 22, 3, 3, 3, 3,
	23, 24, 3, 3, 3, 3, 27, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 21, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 25, 3, 26,
}
var yyTok2 = []int{

	2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
	12, 13, 14, 15, 16, 17, 18, 19, 20,
}
var yyTok3 = []int{
	0,
}

//line yaccpar:1

/*	parser for yacc output	*/

var yyDebug = 0

type yyLexer interface {
	Lex(lval *yySymType) int
	Error(s string)
}

const yyFlag = -1000

func yyTokname(c int) string {
	if c > 0 && c <= len(yyToknames) {
		if yyToknames[c-1] != "" {
			return yyToknames[c-1]
		}
	}
	return fmt.Sprintf("tok-%v", c)
}

func yyStatname(s int) string {
	if s >= 0 && s < len(yyStatenames) {
		if yyStatenames[s] != "" {
			return yyStatenames[s]
		}
	}
	return fmt.Sprintf("state-%v", s)
}

func yylex1(lex yyLexer, lval *yySymType) int {
	c := 0
	char := lex.Lex(lval)
	if char <= 0 {
		c = yyTok1[0]
		goto out
	}
	if char < len(yyTok1) {
		c = yyTok1[char]
		goto out
	}
	if char >= yyPrivate {
		if char < yyPrivate+len(yyTok2) {
			c = yyTok2[char-yyPrivate]
			goto out
		}
	}
	for i := 0; i < len(yyTok3); i += 2 {
		c = yyTok3[i+0]
		if c == char {
			c = yyTok3[i+1]
			goto out
		}
	}

out:
	if c == 0 {
		c = yyTok2[1] /* unknown char */
	}
	if yyDebug >= 3 {
		fmt.Printf("lex %U %s\n", uint(char), yyTokname(c))
	}
	return c
}

func yyParse(yylex yyLexer) int {
	var yyn int
	var yylval yySymType
	var yyVAL yySymType
	yyS := make([]yySymType, yyMaxDepth)

	Nerrs := 0   /* number of errors */
	Errflag := 0 /* error recovery flag */
	yystate := 0
	yychar := -1
	yyp := -1
	goto yystack

ret0:
	return 0

ret1:
	return 1

yystack:
	/* put a state and value onto the stack */
	if yyDebug >= 4 {
		fmt.Printf("char %v in %v\n", yyTokname(yychar), yyStatname(yystate))
	}

	yyp++
	if yyp >= len(yyS) {
		nyys := make([]yySymType, len(yyS)*2)
		copy(nyys, yyS)
		yyS = nyys
	}
	yyS[yyp] = yyVAL
	yyS[yyp].yys = yystate

yynewstate:
	yyn = yyPact[yystate]
	if yyn <= yyFlag {
		goto yydefault /* simple state */
	}
	if yychar < 0 {
		yychar = yylex1(yylex, &yylval)
	}
	yyn += yychar
	if yyn < 0 || yyn >= yyLast {
		goto yydefault
	}
	yyn = yyAct[yyn]
	if yyChk[yyn] == yychar { /* valid shift */
		yychar = -1
		yyVAL = yylval
		yystate = yyn
		if Errflag > 0 {
			Errflag--
		}
		goto yystack
	}

yydefault:
	/* default state action */
	yyn = yyDef[yystate]
	if yyn == -2 {
		if yychar < 0 {
			yychar = yylex1(yylex, &yylval)
		}

		/* look through exception table */
		xi := 0
		for {
			if yyExca[xi+0] == -1 && yyExca[xi+1] == yystate {
				break
			}
			xi += 2
		}
		for xi += 2; ; xi += 2 {
			yyn = yyExca[xi+0]
			if yyn < 0 || yyn == yychar {
				break
			}
		}
		yyn = yyExca[xi+1]
		if yyn < 0 {
			goto ret0
		}
	}
	if yyn == 0 {
		/* error ... attempt to resume parsing */
		switch Errflag {
		case 0: /* brand new error */
			yylex.Error("syntax error")
			Nerrs++
			if yyDebug >= 1 {
				fmt.Printf("%s", yyStatname(yystate))
				fmt.Printf("saw %s\n", yyTokname(yychar))
			}
			fallthrough

		case 1, 2: /* incompletely recovered error ... try again */
			Errflag = 3

			/* find a state where "error" is a legal shift action */
			for yyp >= 0 {
				yyn = yyPact[yyS[yyp].yys] + yyErrCode
				if yyn >= 0 && yyn < yyLast {
					yystate = yyAct[yyn] /* simulate a shift of "error" */
					if yyChk[yystate] == yyErrCode {
						goto yystack
					}
				}

				/* the current p has no shift on "error", pop stack */
				if yyDebug >= 2 {
					fmt.Printf("error recovery pops state %d\n", yyS[yyp].yys)
				}
				yyp--
			}
			/* there is no state on the stack with an error shift ... abort */
			goto ret1

		case 3: /* no shift yet; clobber input char */
			if yyDebug >= 2 {
				fmt.Printf("error recovery discards %s\n", yyTokname(yychar))
			}
			if yychar == yyEofCode {
				goto ret1
			}
			yychar = -1
			goto yynewstate /* try again in the same state */
		}
	}

	/* reduction by production yyn */
	if yyDebug >= 2 {
		fmt.Printf("reduce %v in:\n\t%v\n", yyn, yyStatname(yystate))
	}

	yynt := yyn
	yypt := yyp
	_ = yypt // guard against "declared and not used"

	yyp -= yyR2[yyn]
	yyVAL = yyS[yyp+1]

	/* consult goto table to find next state */
	yyn = yyR1[yyn]
	yyg := yyPgo[yyn]
	yyj := yyg + yyS[yyp].yys + 1

	if yyj >= yyLast {
		yystate = yyAct[yyg]
	} else {
		yystate = yyAct[yyj]
		if yyChk[yystate] != -yyn {
			yystate = yyAct[yyg]
		}
	}
	// dummy call; replaced with literal code
	switch yynt {

	case 1:
		//line parse.y:84
		{
			yyVAL.datum = yyS[yypt-0].datum
			parseValue = yyVAL.datum
		}
	case 2:
		//line parse.y:89
		{
			yyVAL.datum = yyS[yypt-0].datum
			parseValue = yyVAL.datum
		}
	case 3:
		//line parse.y:94
		{
	        yyVAL.datum = NewLabel(yyS[yypt-2].datum, yyS[yypt-0].datum)
		}
	case 4:
		//line parse.y:98
		{
			yyVAL.datum = yyS[yypt-1].datum
		}
	case 5:
		//line parse.y:104
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 6:
		//line parse.y:108
		{
	        yyVAL.datum = yyS[yypt-0].datum
	    }
	case 7:
		//line parse.y:112
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 8:
		//line parse.y:116
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 9:
		//line parse.y:120
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 10:
		//line parse.y:125
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 11:
		//line parse.y:131
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 12:
		//line parse.y:137
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 13:
		//line parse.y:141
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 14:
		//line parse.y:147
		{
	        yyVAL.datum = yyS[yypt-1].datum
		}
	case 15:
		//line parse.y:151
		{
	        yyVAL.datum = yyS[yypt-1].datum
		}
	case 16:
		//line parse.y:155
		{
	        yyVAL.datum = listR(yyS[yypt-3].datum, yyS[yypt-1].datum)
		}
	case 17:
		//line parse.y:159
		{
	        yyVAL.datum = listR(yyS[yypt-3].datum, yyS[yypt-1].datum)
		}
	case 18:
		//line parse.y:163
		{
	        yyVAL.datum = yyS[yypt-0].datum
	    }
	case 19:
		//line parse.y:169
		{
	        yyVAL.datum = list1(yyS[yypt-0].datum)
		}
	case 20:
		//line parse.y:173
		{
	        yyVAL.datum = list1R(yyS[yypt-1].datum, yyS[yypt-0].datum)
		}
	case 21:
		//line parse.y:179
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 22:
		//line parse.y:183
		{
	        yyVAL.datum = list0()
		}
	case 23:
		//line parse.y:189
		{
	        yyVAL.datum = list2(SSymbol{"quote"}, yyS[yypt-0].datum)
		}
	case 24:
		//line parse.y:193
		{
	        yyVAL.datum = list2(SSymbol{"quasiquote"}, yyS[yypt-0].datum)
		}
	case 25:
		//line parse.y:197
		{
	        yyVAL.datum = list2(SSymbol{"unquote"}, yyS[yypt-0].datum)
		}
	case 26:
		//line parse.y:201
		{
	        yyVAL.datum = list2(SSymbol{"unquote-splicing"}, yyS[yypt-0].datum)
		}
	case 27:
		//line parse.y:205
		{
	        yyVAL.datum = list2(SSymbol{"syntax"}, yyS[yypt-0].datum)
		}
	case 28:
		//line parse.y:209
		{
	        yyVAL.datum = list2(SSymbol{"quasisyntax"}, yyS[yypt-0].datum)
		}
	case 29:
		//line parse.y:213
		{
	        yyVAL.datum = list2(SSymbol{"unsyntax"}, yyS[yypt-0].datum)
		}
	case 30:
		//line parse.y:217
		{
	        yyVAL.datum = list2(SSymbol{"unsyntax-splicing"}, yyS[yypt-0].datum)
		}
	case 31:
		//line parse.y:223
		{
	        yyVAL.datum = DlistZKZRvector(list1(yyS[yypt-1].datum))
		}
	case 32:
		//line parse.y:229
		{
	        yyVAL.datum = Du8ZKlistZKZRbytevector(list1(yyS[yypt-1].datum))
		}
	}
	goto yystack /* stack new state and value */
}
