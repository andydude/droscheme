
//line parse.y:32
package droscheme

import (
	"fmt"
	"strings"
)

var ret Any // returned value from yyParse
var err error // return value for parsing errors


//line parse.y:46
type	yySymType	struct {
	yys	int;
    datum Any;
    label int; // this is for recursive structures
	token int; // this is for token identifiers
}
const	ID	= 57346
const	BOOL	= 57347
const	NUMBER	= 57348
const	CHAR	= 57349
const	STRING	= 57350
const	LABEL	= 57351
const	VECTORPAREN	= 57352
const	U8VECTORPAREN	= 57353
const	QUOTE	= 57354
const	QQUOTE	= 57355
const	UNQUOTE	= 57356
const	UNQUOTES	= 57357
const	SYNTAX	= 57358
const	QSYNTAX	= 57359
const	UNSYNTAX	= 57360
const	UNSYNTAXS	= 57361
const	ELLIPSIS	= 57362
var	yyToknames	 =[]string {
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
var	yyStatenames	 =[]string {
}
																																						const	yyEofCode	= 1
const	yyErrCode	= 2
const	yyMaxDepth	= 200

//line parse.y:231

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

//line yacctab:1
var	yyExca = []int {
-1, 1,
	1, -1,
	-2, 0,
}
const	yyNprod	= 31
const	yyPrivate	= 57344
var	yyTokenNames []string
var	yyStates []string
const	yyLast	= 77
var	yyAct	= []int {

  30,   1,  46,  49,  47,  45,  43,  26,  27,  16,
  10,  12,  11,   9,   3,   2,   0,   0,   0,  34,
  35,  36,  37,  38,  39,  40,  41,  42,   0,  13,
   5,   6,   7,   8,   4,  17,  14,  18,  19,  20,
  21,  22,  23,  24,  25,  29,  28,  48,  15,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,  32,  31,   0,  33,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,  44,
};
var	yyPact	= []int {

  25,-1000,-1000,-1000, -14,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,  25,  25,-1000,  25,  25,  25,
  25,  25,  25,  25,  25,  25,  25,-1000, -18,-1000,
  25, -19, -23, -20,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,  25,-1000, -21,-1000,
};
var	yyPgo	= []int {

   0,   0,  45,  46,  15,  14,  13,  12,  11,  10,
   9,
};
var	yyR1	= []int {

   0,   1,   1,   1,   1,   4,   4,   4,   4,   4,
   4,   6,   5,   5,   7,   7,   7,   2,   2,   3,
   3,  10,  10,  10,  10,  10,  10,  10,  10,   8,
   9,
};
var	yyR2	= []int {

   0,   1,   1,   3,   2,   1,   1,   1,   1,   1,
   1,   1,   1,   1,   3,   5,   1,   1,   2,   1,
   0,   2,   2,   2,   2,   2,   2,   2,   2,   3,
   3,
};
var	yyChk	= []int {

-1000,  -1,  -4,  -5,   9,   5,   6,   7,   8,  -6,
  -9,  -7,  -8,   4,  11,  23, -10,  10,  12,  13,
  14,  15,  16,  17,  18,  19,  21,  22,  -3,  -2,
  -1,  -3,  -2,  -3,  -1,  -1,  -1,  -1,  -1,  -1,
  -1,  -1,  -1,  24,  -2,  24,  25,  24,  -1,  24,
};
var	yyDef	= []int {

   0,  -2,   1,   2,   0,   5,   6,   7,   8,   9,
  10,  12,  13,  11,  20,  20,  16,  20,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   4,   0,  19,
  17,   0,  19,   0,  21,  22,  23,  24,  25,  26,
  27,  28,   3,  30,  18,  14,   0,  29,   0,  15,
};
var	yyTok1	= []int {

   1,   3,   3,   3,   3,   3,   3,   3,   3,   3,
   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,
   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,
   3,   3,   3,   3,   3,  22,   3,   3,   3,   3,
  23,  24,   3,   3,   3,   3,  25,   3,   3,   3,
   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,
   3,  21,
};
var	yyTok2	= []int {

   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,
  12,  13,  14,  15,  16,  17,  18,  19,  20,
};
var	yyTok3	= []int {
   0,
 };

//line yaccpar:1

/*	parser for yacc output	*/

var yyDebug = 0

type yyLexer interface {
	Lex(lval *yySymType) int
	Error(s string)
}

const yyFlag = -1000

func yyTokname(yyc int) string {
	if yyc > 0 && yyc <= len(yyToknames) {
		if yyToknames[yyc-1] != "" {
			return yyToknames[yyc-1]
		}
	}
	return fmt.Sprintf("tok-%v", yyc)
}

func yyStatname(yys int) string {
	if yys >= 0 && yys < len(yyStatenames) {
		if yyStatenames[yys] != "" {
			return yyStatenames[yys]
		}
	}
	return fmt.Sprintf("state-%v", yys)
}

func yylex1(yylex yyLexer, lval *yySymType) int {
	var yychar int
	var c int

	yychar = yylex.Lex(lval)
	if yychar <= 0 {
		c = yyTok1[0]
		goto out
	}
	if yychar < len(yyTok1) {
		c = yyTok1[yychar]
		goto out
	}
	if yychar >= yyPrivate {
		if yychar < yyPrivate+len(yyTok2) {
			c = yyTok2[yychar-yyPrivate]
			goto out
		}
	}
	for i := 0; i < len(yyTok3); i += 2 {
		c = yyTok3[i+0]
		if c == yychar {
			c = yyTok3[i+1]
			goto out
		}
	}
	c = 0

out:
	if c == 0 {
		c = yyTok2[1] /* unknown char */
	}
	if yyDebug >= 3 {
		fmt.Printf("lex %.4x %s\n", uint(yychar), yyTokname(c))
	}
	return c
}

func yyParse(yylex yyLexer) int {
	var yyn int
	var yylval yySymType
	var YYVAL yySymType
	YYS := make([]yySymType, yyMaxDepth)

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
	if yyp >= len(YYS) {
		nyys := make([]yySymType, len(YYS)*2)
		copy(nyys, YYS)
		YYS = nyys
	}
	YYS[yyp] = YYVAL
	YYS[yyp].yys = yystate

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
		YYVAL = yylval
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
		yyxi := 0
		for {
			if yyExca[yyxi+0] == -1 && yyExca[yyxi+1] == yystate {
				break
			}
			yyxi += 2
		}
		for yyxi += 2; ; yyxi += 2 {
			yyn = yyExca[yyxi+0]
			if yyn < 0 || yyn == yychar {
				break
			}
		}
		yyn = yyExca[yyxi+1]
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
				yyn = yyPact[YYS[yyp].yys] + yyErrCode
				if yyn >= 0 && yyn < yyLast {
					yystate = yyAct[yyn] /* simulate a shift of "error" */
					if yyChk[yystate] == yyErrCode {
						goto yystack
					}
				}

				/* the current yyp has no shift onn "error", pop stack */
				if yyDebug >= 2 {
					fmt.Printf("error recovery pops state %d, uncovers %d\n",
						YYS[yyp].yys, YYS[yyp-1].yys)
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
	_ = yypt		// guard against "declared and not used"

	yyp -= yyR2[yyn]
	YYVAL = YYS[yyp+1]

	/* consult goto table to find next state */
	yyn = yyR1[yyn]
	yyg := yyPgo[yyn]
	yyj := yyg + YYS[yyp].yys + 1

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
	YYVAL.datum = YYS[yypt-0].datum
	ret = YYVAL.datum
	}
case 2:
//line parse.y:90
{
	YYVAL.datum = YYS[yypt-0].datum
	ret = YYVAL.datum
	}
case 3:
//line parse.y:95
{
	//$$.label = $1
	//$$.datum = $3
	}
case 4:
//line parse.y:100
{
	//$$.label = $1
	}
case 5:
//line parse.y:106
{
	YYVAL.datum = YYS[yypt-0].datum
	}
case 6:
//line parse.y:110
{
	YYVAL.datum = YYS[yypt-0].datum
	}
case 7:
//line parse.y:114
{
	YYVAL.datum = YYS[yypt-0].datum
	}
case 8:
//line parse.y:118
{
	YYVAL.datum = YYS[yypt-0].datum
	}
case 9:
//line parse.y:122
{
	YYVAL.datum = YYS[yypt-0].datum
	}
case 10:
//line parse.y:127
{
	YYVAL.datum = YYS[yypt-0].datum
	}
case 11:
//line parse.y:133
{
	YYVAL.datum = YYS[yypt-0].datum
	}
case 12:
//line parse.y:139
{
    YYVAL.datum = YYS[yypt-0].datum
	}
case 13:
//line parse.y:143
{
    YYVAL.datum = YYS[yypt-0].datum
	}
case 14:
//line parse.y:149
{
    YYVAL.datum = YYS[yypt-1].datum
	}
case 15:
//line parse.y:153
{
	YYVAL.datum = listR(YYS[yypt-3].datum, YYS[yypt-1].datum)
	}
case 16:
//line parse.y:157
{
	YYVAL.datum = YYS[yypt-0].datum
    }
case 17:
//line parse.y:163
{
	YYVAL.datum = list1(YYS[yypt-0].datum)
	}
case 18:
//line parse.y:167
{
	YYVAL.datum = list1R(YYS[yypt-1].datum, YYS[yypt-0].datum)
	}
case 19:
//line parse.y:173
{
	YYVAL.datum = YYS[yypt-0].datum
	}
case 20:
//line parse.y:177
{
	YYVAL.datum = list0()
	}
case 21:
//line parse.y:183
{
	YYVAL.datum = list2(SSymbol{"quote"}, YYS[yypt-0].datum)
	}
case 22:
//line parse.y:187
{
	YYVAL.datum = list2(SSymbol{"quasiquote"}, YYS[yypt-0].datum)
	}
case 23:
//line parse.y:191
{
	YYVAL.datum = list2(SSymbol{"unquote"}, YYS[yypt-0].datum)
	}
case 24:
//line parse.y:195
{
	YYVAL.datum = list2(SSymbol{"unquote-splicing"}, YYS[yypt-0].datum)
	}
case 25:
//line parse.y:199
{
	YYVAL.datum = list2(SSymbol{"syntax"}, YYS[yypt-0].datum)
	}
case 26:
//line parse.y:203
{
	YYVAL.datum = list2(SSymbol{"quasisyntax"}, YYS[yypt-0].datum)
	}
case 27:
//line parse.y:207
{
	YYVAL.datum = list2(SSymbol{"unsyntax"}, YYS[yypt-0].datum)
	}
case 28:
//line parse.y:211
{
	YYVAL.datum = list2(SSymbol{"unsyntax-splicing"}, YYS[yypt-0].datum)
	}
case 29:
//line parse.y:217
{
	YYVAL.datum = DlistZKZRvector(list1(YYS[yypt-1].datum))
	}
case 30:
//line parse.y:223
{
	YYVAL.datum = Du8ZKlistZKZRbytevector(list1(YYS[yypt-1].datum))
	}
	}
	goto yystack /* stack new state and value */
}
