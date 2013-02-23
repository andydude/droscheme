
//line parse.y:33
package ds_any_syntax

import (
.	"ds/any"
ds	"ds/any/runtime"
	"fmt"
)


//line parse.y:44
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
const KSYMBOL = 57363
const KEYWORD = 57364
const COMMENT = 57365

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
	"KSYMBOL",
	"KEYWORD",
	"COMMENT",
}
var yyStatenames = []string{}

const yyEofCode = 1
const yyErrCode = 2
const yyMaxDepth = 200

//line parse.y:278

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

//line yacctab:1
var yyExca = []int{
	-1, 1,
	1, -1,
	-2, 0,
}

const yyNprod = 41
const yyPrivate = 57344

var yyTokenNames []string
var yyStates []string

const yyLast = 96

var yyAct = []int{

	41, 1, 67, 62, 64, 32, 33, 34, 63, 65,
	61, 60, 57, 35, 36, 6, 22, 37, 38, 13,
	15, 14, 12, 5, 3, 48, 49, 50, 51, 52,
	53, 54, 55, 2, 0, 0, 56, 18, 8, 9,
	10, 11, 7, 23, 19, 24, 25, 26, 27, 28,
	29, 30, 31, 40, 4, 16, 17, 59, 39, 20,
	34, 21, 43, 66, 0, 0, 0, 0, 68, 0,
	0, 0, 0, 0, 44, 44, 0, 0, 0, 42,
	45, 0, 47, 0, 46, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 58,
}
var yyPact = []int{

	33, -1000, -1000, -1000, 33, 33, 33, -11, -1000, -1000,
	-1000, -1000, -1000, -1000, -1000, -1000, 33, 33, -1000, 33,
	33, 33, -1000, 33, 33, 33, 33, 33, 33, 33,
	33, 33, -1000, -1000, -1000, 33, -1000, -1000, -1000, -15,
	-1000, 33, -16, -17, -27, -21, -25, -18, -1000, -1000,
	-1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, 33,
	-1000, -1000, 33, -1000, -1000, -1000, -28, 33, -1000,
}
var yyPgo = []int{

	0, 0, 62, 53, 58, 33, 24, 23, 22, 21,
	20, 19, 16, 15,
}
var yyR1 = []int{

	0, 1, 1, 1, 1, 1, 1, 1, 7, 13,
	8, 5, 5, 5, 5, 5, 5, 6, 6, 9,
	9, 9, 9, 9, 2, 2, 3, 3, 3, 4,
	4, 12, 12, 12, 12, 12, 12, 12, 12, 10,
	11,
}
var yyR2 = []int{

	0, 1, 1, 2, 2, 2, 3, 2, 2, 2,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 3,
	3, 3, 3, 1, 3, 5, 1, 2, 2, 1,
	0, 2, 2, 2, 2, 2, 2, 2, 2, 3,
	3,
}
var yyChk = []int{

	-1000, -1, -5, -6, 21, -7, -13, 9, 5, 6,
	7, 8, -8, -11, -9, -10, 22, 23, 4, 11,
	26, 28, -12, 10, 12, 13, 14, 15, 16, 17,
	18, 19, -1, -1, -1, 24, 25, -1, -1, -4,
	-3, -1, -4, -2, -3, -4, -2, -4, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, 27, -3, -13,
	27, 27, 30, 29, 29, 27, -1, 30, -1,
}
var yyDef = []int{

	0, -2, 1, 2, 0, 0, 0, 0, 11, 12,
	13, 14, 15, 16, 17, 18, 0, 0, 10, 30,
	30, 30, 23, 30, 0, 0, 0, 0, 0, 0,
	0, 0, 3, 4, 5, 0, 7, 8, 9, 0,
	29, 26, 0, 0, 29, 0, 0, 0, 31, 32,
	33, 34, 35, 36, 37, 38, 6, 40, 27, 28,
	19, 21, 0, 20, 22, 39, 24, 0, 25,
}
var yyTok1 = []int{

	1, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 25, 3, 3, 3, 3,
	26, 27, 3, 3, 3, 3, 30, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 24, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 28, 3, 29,
}
var yyTok2 = []int{

	2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
	12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
	22, 23,
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
		//line parse.y:85
		{
			yyVAL.datum = yyS[yypt-0].datum
	        yylex.(*Lexer).value = yyVAL.datum
		}
	case 2:
		//line parse.y:90
		{
			yyVAL.datum = yyS[yypt-0].datum
	        yylex.(*Lexer).value = yyVAL.datum
		}
	case 3:
		//line parse.y:95
		{
			yyVAL.datum = ds.NewSymbol("#%" + yyS[yypt-0].datum.(Named).Name())
	        yylex.(*Lexer).value = yyVAL.datum
		}
	case 4:
		//line parse.y:100
		{
			yyVAL.datum = ds.NewKeyword(yyS[yypt-1].datum, yyS[yypt-0].datum)
	        yylex.(*Lexer).value = yyVAL.datum
		}
	case 5:
		//line parse.y:105
		{
			yyVAL.datum = yyS[yypt-0].datum
	        yylex.(*Lexer).value = yyVAL.datum
		}
	case 6:
		//line parse.y:110
		{
	        yyVAL.datum = NewLabel(yyS[yypt-2].datum, yyS[yypt-0].datum)
	        yylex.(*Lexer).value = yyVAL.datum
		}
	case 7:
		//line parse.y:115
		{
			yyVAL.datum = yyS[yypt-1].datum
	        yylex.(*Lexer).value = yyVAL.datum
		}
	case 8:
		//line parse.y:122
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 9:
		//line parse.y:127
		{
		}
	case 10:
		//line parse.y:132
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 11:
		//line parse.y:138
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 12:
		//line parse.y:142
		{
	        yyVAL.datum = yyS[yypt-0].datum
	    }
	case 13:
		//line parse.y:146
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 14:
		//line parse.y:150
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 15:
		//line parse.y:154
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 16:
		//line parse.y:158
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 17:
		//line parse.y:164
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 18:
		//line parse.y:168
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 19:
		//line parse.y:174
		{
	        yyVAL.datum = yyS[yypt-1].datum
		}
	case 20:
		//line parse.y:178
		{
	        yyVAL.datum = yyS[yypt-1].datum
		}
	case 21:
		//line parse.y:182
		{
	        yyVAL.datum = yyS[yypt-1].datum
		}
	case 22:
		//line parse.y:186
		{
	        yyVAL.datum = yyS[yypt-1].datum
		}
	case 23:
		//line parse.y:190
		{
	        yyVAL.datum = yyS[yypt-0].datum
	    }
	case 24:
		//line parse.y:196
		{
	        yyVAL.datum = _listZI(yyS[yypt-2].datum, yyS[yypt-0].datum)
		}
	case 25:
		//line parse.y:200
		{
	        if _length(yyS[yypt-4].datum).(int) != 1 {
	            panic("double-dotted-lists must have 3 elements")
	        }
	        yyVAL.datum = _list(yyS[yypt-2].datum, _car(yyS[yypt-4].datum), yyS[yypt-0].datum)
		}
	case 26:
		//line parse.y:209
		{
	        yyVAL.datum = _list(yyS[yypt-0].datum)
		}
	case 27:
		//line parse.y:213
		{
	        yyVAL.datum = _cons(yyS[yypt-1].datum, yyS[yypt-0].datum)
		}
	case 28:
		//line parse.y:217
		{
	        yyVAL.datum = _list(yyS[yypt-1].datum)
		}
	case 29:
		//line parse.y:223
		{
	        yyVAL.datum = yyS[yypt-0].datum
		}
	case 30:
		//line parse.y:227
		{
	        yyVAL.datum = _null()
		}
	case 31:
		//line parse.y:233
		{
	        yyVAL.datum = _list(ds.NewSymbol("quote"), yyS[yypt-0].datum)
		}
	case 32:
		//line parse.y:237
		{
	        yyVAL.datum = _list(ds.NewSymbol("quasiquote"), yyS[yypt-0].datum)
		}
	case 33:
		//line parse.y:241
		{
	        yyVAL.datum = _list(ds.NewSymbol("unquote"), yyS[yypt-0].datum)
		}
	case 34:
		//line parse.y:245
		{
	        yyVAL.datum = _list(ds.NewSymbol("unquote-splicing"), yyS[yypt-0].datum)
		}
	case 35:
		//line parse.y:249
		{
	        yyVAL.datum = _list(ds.NewSymbol("syntax"), yyS[yypt-0].datum)
		}
	case 36:
		//line parse.y:253
		{
	        yyVAL.datum = _list(ds.NewSymbol("quasisyntax"), yyS[yypt-0].datum)
		}
	case 37:
		//line parse.y:257
		{
	        yyVAL.datum = _list(ds.NewSymbol("unsyntax"), yyS[yypt-0].datum)
		}
	case 38:
		//line parse.y:261
		{
	        yyVAL.datum = _list(ds.NewSymbol("unsyntax-splicing"), yyS[yypt-0].datum)
		}
	case 39:
		//line parse.y:267
		{
	        yyVAL.datum = _listZKZRvector(yyS[yypt-1].datum)
		}
	case 40:
		//line parse.y:273
		{
	        yyVAL.datum = _u8ZKlistZKZRbytevector(yyS[yypt-1].datum)
		}
	}
	goto yystack /* stack new state and value */
}
