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
	"bufio"
	"fmt"
	"io"
	"os"
	"time"
)

var gLibraries  map[Any]Any
var gParameters map[Any]Any

var (
    // (make-parameter (standard-error-port))
	gError  = DmakeZKparameter(list1(DstandardZKerrorZKport(list0())))

    // (make-parameter (standard-input-port))
	gInput  = DmakeZKparameter(list1(DstandardZKinputZKport(list0())))

    // (make-parameter (standard-output-port))
	gOutput = DmakeZKparameter(list1(DstandardZKoutputZKport(list0())))
)

/*
 * Procedures of the form K<mangled name> recieve arguments as
 *   kw = <keyword>
 *   st = (<arg1> ... <argn>)
 * Procedures of the form D<mangled name> recieve arguments as
 *   a = (<arg1> ... <argn>)
 */

// (apply-syntax keyword list)
func KapplyZKsyntax(kw, st Any, env *Env) Any {
	cas, cds := unlist2(st)
	return env.Ref(cas).(Transformer).Transform(cas, cds, env)
}

// (begin ...)
func Kbegin(kw, st Any, env *Env) Any {
	if kw.(SSymbol).name != "begin" {
		panic("begin expected begin")
	}
	if !IsPair(st) {
		return Void()
	}
	car, cdr := unlist1R(st)
	value := Deval(list2(car, env))
	if IsNull(cdr) {
		return value
	}
	return Kbegin(kw, cdr, env)
}

// (define var)
// (define var expr)
// (define (var formals) body)
// (define (var . formals) body)
func Kdefine(kw, st Any, env *Env) Any {
	symbol, rest := unlist1R(st)
	return env.Define(symbol, rest)
}

// (define-library (module name) ...) // R7RS
func KdefineZKlibrary(kw, st Any, env *Env) Any {
	// TODO
	//cenv := env.Extend()
	////names := DlistZKref(list2(st, Sint64(0)))
	//exprt := DlistZKref(list2(st, Sint64(1)))
	////imprt := DlistZKref(list2(st, Sint64(2)))
	//begin := DlistZKref(list2(st, Sint64(3)))
	//Deval(list2(begin, cenv))
	//exprtIds := exprt.(SPair).cdr.(SPair).ToVector()
	//for _, symbol := range exprtIds.(SVector).it {
	//	id := symbol.(SSymbol).String()
	//	env.bound[id] = cenv.bound[id]
	//}
	return Void()
}

func KdefineZKmacro(kw, st Any, env *Env) Any {
	symbol, rest := unlist1R(st)
	return env.DefMacro(symbol, rest)
}

func KdefineZKsyntax(kw, st Any, env *Env) Any {
	symbol, rules := unlist2(st)
	return env.DefSyntax(symbol, Deval(list2(rules, env)))
}

func KdefineZKvalues(kw, st Any, env *Env) Any {
	symbols, expr := unlist2(st)
	values := Deval(list2(expr, env))
	cus, cur := symbols, values
	for IsPair(cus) {
		env.Define(cus.(*List).car, list1(cur.(SValues).First()))
		cus = cus.(*List).cdr
		cur = cur.(SValues).Rest()
	}
	return Void()
}

func Kdo(kw, st Any, env *Env) Any {
	// TODO
	return Void()
}

// (dump-environment) -- for debug only
func KdumpZKenvironment(kw, st Any, env *Env) Any {
	env.dump()
	return Void()
}

// (dump-internals) -- for debug only
func KdumpZKinternals(kw, st Any, env *Env) Any {
	DumpInternals()
	return Void()
}

// (if c texpr)
// (if c texpr fexpr)
func Kif(kw, st Any, env *Env) Any {
	// technically this is unlist2O
	test, texpr, rest := unlist2R(st)
	c := Deval(list2(test, env))
	if !IsBool(c) || bool(c.(SBool)) {
		return Deval(list2(texpr, env))
	}
	if IsPair(rest) {
		fexpr := unlist1(rest)
		return Deval(list2(fexpr, env))
	}
	return Void()
}

func KcaseZKlambda(kw, st Any, env *Env) Any {
	// TODO
	return Void()
}

// (current-environment) -- for debug only
func KcurrentZKenvironment(kw, st Any, env *Env) Any {
	return env
}

// (lambda var body)
// (lambda (formals) body)
// (lambda (formals . var) body)
func Klambda(kw, st Any, env *Env) Any {
	if kw.(SSymbol).name != "lambda" {
		panic("lambda expected lambda")
	}
	form, body := unlist1R(st)
	return SProc{form: form, body: body, env: env}
}

// (let)
func Klet(kw, st Any, env *Env) Any {
	binds, body := unlist1R(st)

	if IsSymbol(binds) { // let loop
		// TODO
	}

	// separate variables and values
	bvars, bvals := bindingsToPair(binds)

	// evaluate each item of the list
	bvals = bvals.(Evaler).Eval(env)

	// the invisible lambda
	lam := NewLambda(list1R(bvars, body), env)
	return Dapply(list2(lam, bvals))
}

// (let*)
func KletZH(kw, st Any, env *Env) Any {
	binds, body := unlist1R(st)
	if IsNull(binds) {
		return Klet(kw, st, env)
	}
	cenv := env.Extend()

	// separate variables and values
	bvars, bvals := bindingsToPair(binds)
	vvars := listToVector(bvars)
	vvals := listToVector(bvals)
	for k, _ := range vvars {
		cenv.Define(vvars[k], list1(Deval(list2(vvals[k], cenv))))
	}

	return NewBegin(body, cenv)
}

// (letrec)
func Kletrec(kw, st Any, env *Env) Any {
	// TODO
	return Void()
}

// (let-values)
func KletZKvalues(kw, st Any, env *Env) Any {
	// TODO
	return Void()
}

// (letrec-values)
func KletrecZKvalues(kw, st Any, env *Env) Any {
	// TODO
	return Void()
}

// (library (module name) ...) // R6RS
func Klibrary(kw, st Any, env *Env) Any {
	// TODO
	return Void()
}

func Kunquote(kw, st Any, env *Env) Any {
	return listToValues(Deval(list2(list1R(NewSymbol("list"), st), env)))
}

func KunquoteZKsplicing(kw, st Any, env *Env) Any {
	return listToValues(Deval(list2(list1R(NewSymbol("append"), st), env)))
}

// (quasiquote expr)
func Kquasiquote(kw, st Any, env *Env) Any {
	expr := unlist1(st)
	if IsPair(expr) {
		cas, cds := unlist1R(expr)
		if IsPair(cas) {
			caas, cdas := unlist1R(cas)
			if IsSymbol(caas) {
				switch id := caas.(SSymbol).name; id {
				case "quasiquote":
					return expr
				case "quote":
					return expr
				case "unquote":
					values := Kunquote(caas, cdas, env)
					return listR(valuesToList(values), Kquasiquote(kw, list1(cds), env))
				case "unquote-splicing":
					values := KunquoteZKsplicing(caas, cdas, env)
					return listR(valuesToList(values), Kquasiquote(kw, list1(cds), env))
				}
			}
			return list1R(
				Kquasiquote(kw, list1(cas), env), 
				Kquasiquote(kw, list1(cds), env))
		}
		return list1R(cas, Kquasiquote(kw, list1(cds), env))
	}
	return expr
}

// (quote expr)
func Kquote(kw, st Any, env *Env) Any {
	return unlist1(st)
}

// (set! var expr)
func KsetZA(kw, st Any, env *Env) Any {
	symbol, value := unlist2(st)
	value = Deval(list2(value, env))
	return env.Set(symbol, value)
}

func KsyntaxZKcase(kw, st Any, env *Env) Any {
	//expr, literals, body := unlist2R(st)
	//return SCaseSyntax{expr: expr, lits: literals, body: body, env: env}
	return Void()
}

func KsyntaxZKrules(kw, st Any, env *Env) Any {
	literals, body := unlist1R(st)
	return SRuleSyntax{lits: literals, body: body, env: env}
}

/*
 * These function identifiers are designed to be isomorphic to scheme identifiers.
 *
 * For every scheme id, we can calculate the Go id as follows:
 *   (1) replace and punctuation with the 'Z*' pattern, (2) prepend 'D'
 * For every Go id in this file, we can calculate the scheme id as follows:
 *   (1) remove the 'D' (2) replace any 'Z*' pattern with the table below.
 *
 * Encoding System:
 *   ZA = '!'
 *   ZB = reserved for QUOTATION MARK
 *   ZC = '#'
 *   ZD = '$'
 *   ZE = '%'
 *   ZF = '&'
 *   ZG = reserved for APOSTROPHE
 *   ZH = '*'
 *   ZI = '+'
 *   ZJ = ','
 *   ZK = '-'
 *   ZL = '.'
 *   ZM = '/'
 *   ZN = ':'
 *   ZO = ';'
 *   ZP = '<'
 *   ZQ = '='
 *   ZR = '>'
 *   ZS = '?'
 *   ZT = '@'
 *   ZU = '\'
 *   ZV = '^'
 *   ZW = '`'
 *   ZX = '|'
 *   ZY = '~'
 *   ZZ = 'Z'
 */

///* (*) -- derived, but useful
// *
// * (define (* . rest)
// *   (fold-right num* 1 rest))
// */
//func DZH(a Any) Any {
//	return DfoldZKright(list3(NewPrim(DnumZH), Sint64(1), a))
//}
//
///* (+) -- derived, but useful
// *
// * (define (+ . rest)
// *   (fold-right num+ 0 rest))
// */
//func DZI(a Any) Any {
//	return DfoldZKright(list3(NewPrim(DnumZI), Sint64(0), a))
//}
//
///* (-) -- derived, but useful
// *
// * (define -
// *   (case-lambda
// *     (() 0)
// *     ((a) (num- 0 a))
// *     ((a . rest) (num- a (+ . rest)))))
// */
//func DZK(a Any) Any {
//	if IsNull(a) {
//		return Sint64(0)
//	}
//	x, ys := unlist1R(a)
//	if IsNull(ys) {
//		return DnumZK(list2(Sint64(0), x))
//	}
//	return DnumZK(list2(x, DZI(ys)))
//}
//
///* (/) -- derived, but useful
// *
// * (define /
// *   (case-lambda
// *     (() 1)
// *     ((a) (num/ 1 a))
// *     ((a . rest) (num/ a (* . rest)))))
// */
//func DZM(a Any) Any {
//	if IsNull(a) {
//		return Sint64(1)
//	}
//	x, ys := unlist1R(a)
//	if IsNull(ys) {
//		return DnumZM(list2(Sint64(1), x))
//	}
//	return DnumZM(list2(x, DZH(ys)))
//}

func Dacos(a Any) Any {
	return unlist1(a).(TrigNum).ArcCos()
}

func Dangle(a Any) Any {
	return unlist1(a).(ComplexNum).Angle()
}

// (apply proc arg1 ... restargs)
func Dapply(a Any) Any {
	proc, rest := unlist1R(a)
	return proc.(Applier).Apply(DlistZH(rest))
}

func Dasin(a Any) Any {
	return unlist1(a).(TrigNum).ArcSin()
}

func Datan(a Any) Any {
	// CLEANUP
	xa, ya := unlist1R(a)
	x, y := Sfloat64(ToFlonum(xa.(Num))), Sfloat64(1)
	if !IsNull(ya) {
		y = Sfloat64(ToFlonum(unlist1(ya).(Num)))
	}
	return x.ArcTan(y)
}

func DbinaryZKportZS(a Any) Any {
	return SBool(IsBinaryPort(unlist1(a)))
}

func DbooleanZQZS(a Any) Any {
	b, c := unlist2(a)
	return SBool(Equal(b, c))
}

func DbooleanZS(a Any) Any {
	return SBool(IsBool(unlist1(a)))
}

func DbytevectorZKcopy(a Any) Any {
	frv := unlist1(a)
	tov := DmakeZKbytevector(list1(DbytevectorZKlength(a)))
	DbytevectorZKcopyZA(list2(frv, tov))
	return tov
}

func DbytevectorZKcopyZA(a Any) Any {
	from, to := unlist2(a)
	var s = Sint64(0)
	var e = DbytevectorZKlength(list1(from))
	DbytevectorZKcopyZKpartialZA(list3R(from, s, e, list2(to, s)))
	return Void()
}

func DbytevectorZKcopyZKpartial(a Any) Any {
	frv, s, e := unlist3(a)
	tov := DmakeZKbytevector(list1(DbytevectorZKlength(list1(frv))))
	DbytevectorZKcopyZKpartialZA(list3R(frv, s, e, list2(tov, Sint64(0))))
	return tov
}

func DbytevectorZKcopyZKpartialZA(a Any) Any {
	from, str, end, rest := unlist3R(a)
	to, ta := unlist2(rest)
	//fmt.Printf("%s, %s, %s, %s, %s, %s\n", from, str, end, rest, to, ta)
	frv := from.(SBinary)
	tov := to.(SBinary)
	var t = int(ToFixnum(ta))
	var s = int(ToFixnum(str))
	var e = int(ToFixnum(end))
	for i := s; i < e; i++ {
		tov[i + t - s] = frv[i]
	}
	return Void()
}

func DbytevectorZKlength(a Any) Any {
	return Sint64(len(unlist1(a).(SBinary)))
}

func DbytevectorZKu8(a Any) Any {
	return Du8ZKlistZKZRbytevector(list1(a))
}

func DbytevectorZKu8ZKref(a Any) Any {
	o, k := unlist2(a)
	return o.(SBinary).Ref(k)
}

func DbytevectorZKu8ZKsetZA(a Any) Any {
	o, k, v := unlist3(a)
	return o.(SBinary).Set(k, v)
}

// R6RS:bytevector->u8-list
func DbytevectorZKZRu8ZKlist(a Any) Any {
	return DvectorZKZRlist(list1(DbytevectorZKZRu8ZKvector(a)))
}

func DbytevectorZKZRu8ZKvector(a Any) Any {
	bvec := unlist1(a).(SBinary)
	blen := len(bvec)
	vany := DmakeZKvector(list1(Sint64(blen)))
	v := vany.(SVector)
	for i := 0; i < blen; i++ {
		v[i] = Sint64(bvec[i])
	}
	return vany
}

func DbytevectorZS(a Any) Any {
	return SBool(IsBinary(unlist1(a)))
}

func DcallZKwithZKcomposableZKcontinuation(a Any) Any {
	// TODO
	return Void()
}

func DcallZKwithZKcurrentZKcontinuation(a Any) Any {
	return DcallZKwithZKescapeZKcontinuation(a) // TODO
}

func DcallZKwithZKescapeZKcontinuation(a Any) (value Any) {
	f := unlist1(a)
	c := SCont{code: Hash(f)}
	defer func () {
		x := recover()
		if IsContinuation(x) && c.Equal(x.(SCont)) {
			value = Dvalues(x.(SCont).it)
		} else {
			panic(x)
		}
	}()
	Dapply(list2(f, list1(c)))
	return Void()
}

func DcallZKwithZKport(a Any) Any {
	port, proc := unlist2(a)
	if !IsPort(port) {
		panic(newTypeError("call-with-port expected port"))
	}
	defer DcloseZKport(list1(port))
	return proc.(Applier).Apply(list1(port))
}

func DcallZKwithZKvalues(a Any) Any {
	producer, consumer := unlist2(a)
	values := producer.(Applier).Apply(list0())
	return consumer.(Applier).Apply(valuesToList(values))
}

func DcallZMcc(a Any) Any {
	return DcallZKwithZKcurrentZKcontinuation(a)
}

func Dcar(a Any) Any {
	return unlist1(a).(*List).car
}

func Dcdr(a Any) Any {
	return unlist1(a).(*List).cdr
}

func DcharZKZRinteger(a Any) Any {
	return Sint64(unlist1(a).(SChar))
}

func DcharZKreadyZS(a Any) Any {
	port := unlist0O(a, DcurrentZKinputZKport(list0()))
	_, _, err := port.(RunePeeker).PeekRune()
	if err != nil {
		return SBool(false)
	}
	return SBool(true)
}

func DcharZS(a Any) Any {
	return SBool(IsChar(unlist1(a)))
}

func DcloseZKinputZKport(a Any) Any {
	port := unlist1(a)
	if !IsPort(port) {
		panic(newTypeError("close-input-port expected port"))
	}
	if !IsInputPort(port) {
		panic(newTypeError("close-input-port expected input-port"))
	}
	return DcloseZKport(a)
}

func DcloseZKoutputZKport(a Any) Any {
	port := unlist1(a)
	if !IsPort(port) {
		panic(newTypeError("close-output-port expected port"))
	}
	if !IsOutputPort(port) {
		panic(newTypeError("close-output-port expected output-port"))
	}
	return DcloseZKport(a)
}

func DcloseZKport(a Any) Any {
	err := unlist1(a).(Port).Close()
	if err != nil {
		panic(err)
	}
	return Void()
}

func DcomplexZS(a Any) Any {
	return SBool(IsComplex(unlist1(a)))
}

func DcontinuationZKcapture(a Any) Any {
	return Void()
}

func DcontinuationZKreturn(a Any) Any {
	return Void()
}

func Dcons(a Any) Any {
	return list1R(unlist2(a))
}

func Dcos(a Any) Any {
	return unlist1(a).(TrigNum).Cos()
}

func DcurrentZKerrorZKport(a Any) Any {
	return gError.(SPrim).Apply(a)
}

func DcurrentZKinputZKport(a Any) Any {
	return gInput.(SPrim).Apply(a)
}

func DcurrentZKoutputZKport(a Any) Any {
	return gOutput.(SPrim).Apply(a)
}

func DcurrentZKjiffy(a Any) Any {
	return Sint64(time.Now().UnixNano())
}

func DcurrentZKsecond(a Any) Any {
	return Sint64(time.Now().Unix())
}

func Ddenominator(a Any) Any {
	// TODO
	return list0()
}

func Ddisplay(a Any) Any {
	obj, port := unlist1O(a, DcurrentZKoutputZKport(list0()))
	s := fmt.Sprintf("%s", obj.(SString).GoString())
	port.(OPort).Write([]byte(s))
	return Void()
}

func DdroschemeZKrootZKpath(a Any) Any {
	return ToString(GetRootPath())
}

func DdroschemeZKfloat32ZKenvironment(a Any) Any {
	// TODO
	return Void()
}

func DdroschemeZKfloat64ZKenvironment(a Any) Any {
	// TODO
	return Void()
}

func DdroschemeZKbranch(a Any) Any {
	go DdroschemeZKwait(list0())
	return Void()
}

func DdroschemeZKwait(a Any) Any {
	c := make(chan bool)
	return SBool(<-c)
}

func DdynamicZKwind(a Any) Any {
	return list0()
}

func DemptyZS(a Any) Any {
	return SBool(IsEmpty(unlist1(a)))
}

func DeofZKobject(a Any) Any {
	return SChar(-1)
}

func DeofZKobjectZS(a Any) Any {
	o := unlist1(a)
	if IsChar(o) {
		return SBool(unlist1(a).(SChar) == -1)
	}
	return SBool(false)
}

func DeqZS(a Any) Any {
	return DeqvZS(a)
}

func DequalZS(a Any) Any {
	var x, y = unlist2(a)
	return SBool(x.Equal(y))
}

func DeqvZS(a Any) Any {
	var x, y = unlist2(a)
	return SBool(Equal(x, y))
}

func Derror(a Any) Any {
	msg, irr := unlist1R(a)
	return Draise(list1(NewError(msg, irr)))
}

func DerrorZKobjectZKirritants(a Any) Any {
	return unlist1(a).(Error).Irritants()
}

func DerrorZKobjectZKmessage(a Any) Any {
	return ToString(unlist1(a).(Error).Error())
}

func DerrorZKobjectZS(a Any) Any {
	return SBool(IsError(unlist1(a)))
}

// this is to compensate for failing
// to put an .Eval method on every object
// (eval-literal expression environment)
func DevalZKliteral(a Any) Any {
	expr, opt := unlist2(a)
	env := opt.(*Env)

	// check for literals
	if _, ok := expr.(Evaler); !ok {
		return expr
	}
	return expr.(Evaler).Eval(env)
}

// (eval expression environment)
func Deval(a Any) Any {
	expr, opt := unlist2(a)
	env := opt.(*Env)

	// check for nonpairs
	if !IsPair(expr) {
		return DevalZKliteral(a)
	}

	// check if car is syntactic keyword
	cas, cds := unlist1R(expr)
	if IsSymbol(cas) && IsSyntax(cas, env) {
		//fmt.Printf("--SYNTAX%s\n", expr)
		return env.Ref(cas).(Transformer).Transform(cas, cds, env)
	}
	//fmt.Printf("--PROC%s\n", expr)

	// evaluate each argument
	list := DevalZKliteral(a)

	// check if car is procedure
	car, cdr := unlist1R(list)
	if !IsProcedure(car) {
		panic(newTypeError("expected procedure"))
	}
	if _, ok := car.(Applier); !ok {
		panic(newTypeError("expected procedure (Applier)"))
	}

	return car.(Applier).Apply(cdr)
}

func DexactZKZRinexact(a Any) Any {
	return list0()
}

func DexactZKintegerZKsqrt(a Any) Any {
	return list0()
}

func DexactZKintegerZS(a Any) Any {
	return list0()
}

func DexactZS(a Any) Any {
	return SBool(IsExact(unlist1(a)))
}

func Dexp(a Any) Any {
	x := unlist1(a)
	return x.(TrigNum).Exp()
}

func Dexpt(a Any) Any {
	x, y := unlist2Number(a)
	return x.(TrigNum).Pow(y)
}

func Dfloor(a Any) Any {
	return unlist1(a).(RealNum).RTN()
}

/* (fold-left) -- derived, but useful
 *
 * (define fold-left
 *   (case-lambda
 *     ((proc nil) (proc nil))
 *     ((proc nil ls)
 *      (if (null? ls) nil
 *          (fold-left proc (proc nil (car ls)) (cdr ls))))
 *     ((proc nil . lss)
 *      (if (null? (car lss)) nil
 *          (let ((cars (map car lss))
 *                (cdrs (map cdr lss)))
 *            (apply fold-left proc (apply proc
 *              (append (list nil) cars)) cdrs))))))
 */
func DfoldZKleft(a Any) Any {
	//proc, null, rest := unlist2R(a)
	// TODO
	return list0()
}

/* (fold-right) -- derived, but useful
 *
 * (define fold-right
 *   (case-lambda
 *     ((proc nil) (proc nil))
 *     ((proc nil ls)
 *      (if (null? ls) nil
 *          (proc (car ls) (fold-right proc nil (cdr ls)))))
 *     ((proc nil . lss)
 *      (if (null? (car lss)) nil
 *          (let ((cars (map car lss))
 *                (cdrs (map cdr lss)))
 *            (apply proc (append cars (list
 *              (apply fold-right proc nil cdrs)))))))))
 */
func DfoldZKright(a Any) Any {
	proc, null, rest := unlist2R(a)
	if IsNull(rest) {
		return Dapply(list2(proc, list1(null)))
	}
	if ToFixnum(Dlength(list1(rest))) > 1 {
		panic("expected binary procedure")
	}
	ls := unlist1(rest)
	if IsNull(ls) {
		return null
	}
	ca, cd := unlist1R(ls)
	return Dapply(list2(proc, list2(ca, DfoldZKright(list3(proc, null, cd)))))
}

func DflushZKoutputZKport(a Any) Any {
	port := unlist0O(a, DcurrentZKoutputZKport(list0()))
	if !IsPort(port) {
		panic(newTypeError("flush-output-port expected port"))
	}
	if !IsOutputPort(port) {
		panic(newTypeError("flush-output-port expected output-port"))
	}
	err := port.(Flusher).Flush()
	if err != nil {
		panic(err)
	}
	return Void()
}

// (get-output-bytevector port)
func DgetZKoutputZKbytevector(a Any) Any {
	port := unlist1(a)
	if !IsPort(port) {
		panic(newTypeError("get-output-bytevector expected port"))
	}
	if !IsBinaryPort(port) {
		panic(newTypeError("get-output-bytevector expected binary-port"))
	}
	if !IsOutputPort(port) {
		panic(newTypeError("get-output-bytevector expected output-port"))
	}
	if _, ok := port.(*SBytePort); !ok {
		panic(newError("get-output-bytevector expected port created by open-output-bytevector"))
	}
	return port.(*SBytePort).it
}

// (get-output-string port)
func DgetZKoutputZKstring(a Any) Any {
	port := unlist1(a)
	if !IsPort(port) {
		panic(newTypeError("get-output-string expected port"))
	}
	if !IsTextualPort(port) {
		panic(newTypeError("get-output-string expected textual-port"))
	}
	if !IsOutputPort(port) {
		panic(newTypeError("get-output-string expected output-port"))
	}
	if _, ok := port.(*SCharPort); !ok {
		panic(newError("get-output-string expected port created by open-output-string"))
	}
	return port.(*SCharPort).it
}

func Dhash(a Any) Any {
	return Sint64(int64(Hash(unlist1(a))))
}

// (hash->list hashtable) -- Racket
// (hashtable->list hashtable) -- droscheme extension
func DhashtableZKZRlist(a Any) Any {
	return DvectorZKZRlist(list1(DhashtableZKZRvector(a)))
}

func DhashtableZKZRvector(a Any) Any {
	return NewVector(unlist1(a).(STable).Items())
}

// (hashtable-clear! hashtable)
// (hashtable-clear! hashtable k) -- unimplemented
func DhashtableZKclearZA(a Any) Any {
	h, _ := unlist1R(a)
	o := h.(STable).it
	for k, _ := range o {
		delete(o, k)
	}
	return Void()
}

// (hashtable-contains? hashtable key)
func DhashtableZKcontainsZS(a Any) Any {
	v := DhashtableZKref(a)
	return SBool(v != nil)
}

// (hashtable-copy hashtable) -- unimplemented
// (hashtable-copy hashtable mutable) -- unimplemented
func DhashtableZKcopy(a Any) Any {
	// TODO
	return unlist1(a)
}

// (hashtable-delete! hashtable key)
func DhashtableZKdeleteZA(a Any) Any {
	h, k := unlist2(a)
	h.(STable).Delete(k)
	return Void()
}

// (hashtable-entries hashtable)
func DhashtableZKentries(a Any) Any {
	ks, vs := unlist1(a).(STable).Entries()
	return NewValues([]Any{NewVector(ks), NewVector(vs)})
}

func DhashtableZKequivalenceZKfunction(a Any) Any {
	return unlist1(a).(STable).equivFn
}

func DhashtableZKhashZKfunction(a Any) Any {
	return unlist1(a).(STable).hashFn
}

// (hashtable-keys hashtable)
func DhashtableZKkeys(a Any) Any {
	ks := unlist1(a).(STable).Keys()
	return NewVector(ks)
}

// (hashtable-mutable? hashtable)
func DhashtableZKmutableZS(a Any) Any {
	return SBool(true)
}

// (hashtable-ref hashtable key) -- droscheme extension
// (hashtable-ref hashtable key default)
func DhashtableZKref(a Any) Any {
	t, k, d := unlist2O(a, nil)
	v := t.(STable).Ref(k)
	if v == nil {
		return d
	}
	return v
}

// (hashtable-set! hashtable key value)
func DhashtableZKsetZA(a Any) Any {
	t, k, v := unlist3(a)
	t.(STable).Set(k, v)
	return Void()
}

// (hashtable-size hashtable)
func DhashtableZKsize(a Any) Any {
	return Sint64(len(unlist1(a).(STable).it))
}

// (hashtable-update! hashtable key proc) -- droscheme extension
// (hashtable-update! hashtable key proc default)
func DhashtableZKupdateZA(a Any) Any {
	t, k, p, d := unlist3O(a, nil)
	v := DhashtableZKref(list3(t, k, d))
	o := p.(Applier).Apply(list1(v))
	DhashtableZKsetZA(list3(t, k, o))
	return Void()
}

// (hash-values hashtable) -- Racket
// (hashtable-values hashtable) -- droscheme extension
func DhashtableZKvalues(a Any) Any {
	vs := unlist1(a).(STable).Values()
	return NewVector(vs)
}

// (hashtable? obj)
func DhashtableZS(a Any) Any {
	return SBool(IsTable(unlist1(a)))
}

func Didentity(a Any) Any {
	return unlist1(a)
}

func DimagZKpart(a Any) Any {
	return unlist1(a).(ComplexNum).Imag()
}

func DinexactZKZRexact(a Any) Any {
	return Void()
}

func DinexactZQZS(a Any) Any {
	b, c := unlist2(a)
	return SBool(Equal(b, c))
}

func DinexactZS(a Any) Any {
	return SBool(IsInexact(unlist1(a)))
}

func DinputZKportZS(a Any) Any {
	return SBool(IsInputPort(unlist1(a)))
}

func DintegerZKZRchar(a Any) Any {
	return SChar(ToFixnum(unlist1(a)))
}

func DintegerZS(a Any) Any {
	return SBool(IsInteger(unlist1(a)))
}

// (interaction-environment)
func DinteractionZKenvironment(a Any) Any {
	return DschemeZKreportZKenvironment(list1(Sint64('D')))
}

func DlastZKpair(a Any) Any {
	if IsNull(unlist1(a)) {
		return list0()
	}
	var cur *List = unlist1(a).(*List)
	for IsPair(cur.cdr) {
		cur = cur.cdr.(*List)
	}
	return cur
}

func Dlength(a Any) Any {
	o := unlist1(a)

	// cycle detection (only needed in mutable model)
	switch {
	case IsNull(o):
		return Sint64(0)
	case IsPair(o):
		return Sint64(1 + ToFixnum(Dlength(list1(o.(*List).cdr))))
	}
	return Sint64(-10000)
}

// (list ...)
func Dlist(a Any) Any {
	// so easy
	return a
}

// (list* ...)
func DlistZH(a Any) Any {
	// so hard
	v := DlistZKZRvector(list1(a))
	return DvectorZKZRlistZH(list1(v))
}

func DlistZKZRstring(a Any) Any {
	return DvectorZKZRstring(list1(DlistZKZRvector(a)))
}

// (list->vector l)
func DlistZKZRvector(a Any) Any {
	var vec = []Any{}
	for cur := unlist1(a); IsPair(cur); cur = cur.(*List).cdr {
		vec = append(vec, cur.(*List).car)
	}
	return NewVector(vec)
}

// (list*->vector l)
func DlistZHZKZRvector(a Any) Any {
	var vec = []Any{}
	var cur Any
	for cur = unlist1(a); IsPair(cur.(*List).cdr); cur = cur.(*List).cdr {
		vec = append(vec, cur.(*List).car)
	}
	vec = append(vec, cur.(*List).car)
	vec = append(vec, cur.(*List).cdr)
	return NewVector(vec)
}

func DlistZKcopy(a Any) Any {
	l := unlist1(a)
	if IsNull(l) {
		return list0()
	}
	car, cdr := unlist1R(l)
	return list1R(car, DlistZKcopy(list1(cdr)))
}

func DlistZKref(a Any) Any {
	list, ka, rest := unlist2R(a)
	k := ToFixnum(ka)
	for cur := list; IsPair(cur); k, cur = k-1, cur.(*List).cdr {
		if k == 0 {
			return cur.(*List).car
		}
	}
	if IsPair(rest) {
		return Dcar(list1(rest))
	}
	panic(newTypeError("list-ref"))
}

func DlistZKtail(a Any) Any {
	list, ka := unlist2(a)
	k := ToFixnum(ka)
	for cur := list; IsPair(cur); k, cur = k-1, cur.(*List).cdr {
		if k == 0 {
			return cur.(*List).cdr
		}
	}
	panic(newTypeError("list-tail"))
}

func DlistZS(a Any) Any {
	ls := unlist1(a)

	// By definition, all lists are chains of pairs that have
	// finite length and are terminated by the empty list. [R6RS]

	// cycle detection (only needed in mutable model)
	switch {
	case IsNull(ls):
		return SBool(true)
	case IsPair(ls):
		return DlistZS(list1(ls.(*List).cdr))
	}

	return SBool(false)
}

func Dload(a Any) Any {
	fs, opt := unlist1R(a)
	filename := fs.(SString).GoString()
	env := EmptyEnv()
	if IsPair(opt) {
		env = unlist1(opt).(*Env)
	}
	value := Void()

	// open
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}

	// slurp
	port := bufio.NewReaderSize(file, 16777216)
	input, err := port.ReadString(eof)
	if err != nil {
		if err != io.EOF {
			panic(err)
		}
		err = nil
	}

	// read
	value, err = Read("(begin "+input+"\n)") // hack
	if err != nil {
		if err != io.EOF {
			panic(err)
		}
		err = nil
	}

	// eval
	_, err = Eval(value, env)
	if err != nil {
		panic(err)
	}

	return Void()
}

func Dlog(a Any) Any {
	xa, ya := unlist1R(a)
	if IsNull(ya) {
		return xa.(TrigNum).Ln()
	}
	x, y := unify(xa.(Num), unlist1(ya).(Num))
	return x.(TrigNum).Log(y)
}

func Dmagnitude(a Any) Any {
	return unlist1(a).(ComplexNum).Scale()
}

func DmakeZM(a Any) Any {
	nany, dany := unlist2(a)
	n := ToFixnum(nany)
	d := ToFixnum(dany)
	return NewRational64(n, d)
}

// (make-bytevector k)
// (make-bytevector k byte)
func DmakeZKbytevector(a Any) Any {
	return Du8ZKvectorZKZRbytevector(list1(DmakeZKvector(a)))
}

func DmakeZKeqZKhashtable(a Any) Any {
	k := unlist0O(a, Sint64(32))
	return MakeTable(NewPrim(Dhash), NewPrim(DeqZS), k)
}

func DmakeZKeqvZKhashtable(a Any) Any {
	k := unlist0O(a, Sint64(32))
	return MakeTable(NewPrim(Dhash), NewPrim(DeqvZS), k)
}

func DmakeZKhashtable(a Any) Any {
	hash, equiv, k := unlist2O(a, Sint64(32))
	return MakeTable(hash, equiv, k)
}

func DmakeZKlist(a Any) Any {
	return DvectorZKZRlist(list1(DmakeZKvector(a)))
}

// (make-parameter init converter?)
func DmakeZKparameter(a Any) Any {
	init, conv := unlist1O(a, NewPrim(Didentity))
	pc := Sint64(getPC(&conv))
	if gParameters == nil {
		gParameters = make(map[Any]Any, 2048)
	}

	// make lambda
	var param = func(c Any) Any {
		cell := gParameters[pc]
		if IsNull(c) {
			return cell
		}
		car, cdr := unlist1R(c)
		if IsNull(cdr) {
			gParameters[pc] = Dapply(list2(conv, list1(car)))
			return Void()
		}
		return Dapply(list2(conv, list1(car)))
	}

	gParameters[pc] = Dapply(list2(conv, list1(init)))
	return SPrim{param, "(make-parameter closure)"}
}

func DmakeZKpolar(a Any) Any {
	return NewComplexPolar(unlist2Number(a))
}

func DmakeZKrectangular(a Any) Any {
	return NewComplex(unlist2Number(a))
}

// (make-string k)
// (make-string k char)
func DmakeZKstring(a Any) Any {
	return DvectorZKZRstring(list1(DmakeZKvector(a)))
}

// (make-vector k fill?)
func DmakeZKvector(a Any) Any {
	k, rest := unlist1R(a)
	var fill Any = SNull{}
	if IsPair(rest) {
		fill = rest.(*List).car
	}
	n := int(ToFixnum(k))
	v := make([]Any, n, 256)
	for i := 0; i < n; i++ {
		v[i] = fill
	}
	return NewVector(v)
}

func DnegativeZS(a Any) Any {
	return SBool(unlist1(a).(RealNum).Cmp(Sfloat64(0)) == -1)
}

func Dnewline(a Any) Any {
	return list0()
}

func Dnot(a Any) Any {
	c := unlist1(a)
	if !IsBool(c) || bool(c.(SBool)) {
		return SBool(false)
	}
	return SBool(true)
}

// (null-environment version)
func DnullZKenvironment(a Any) Any {
	env := EmptyEnv()
	switch v := ToFixnum(unlist1(a)); {
	case v == 0:
		return env
	case v == 'D':
		env.registerSyntax(KapplyZKsyntax)
		env.registerSyntax(KcurrentZKenvironment)
		env.registerSyntax(KdefineZKmacro)
		env.registerSyntax(KdumpZKenvironment)
		env.registerSyntax(KdumpZKinternals)
		fallthrough
	case v == 7:
		env.registerSyntax(KdefineZKlibrary)
		//env.registerSyntax(KdefineZKrecordZKtype)
		env.registerSyntax(KdefineZKvalues)
		//env.registerSyntax(Kguard)
		//env.registerSyntax(Kparameterize)
		fallthrough
	case v == 6:
		//env.registerSyntax(Kassert)
		env.registerSyntax(KcaseZKlambda)
		//env.registerSyntax(KidentifierZKsyntax)
		env.registerSyntax(Klibrary)
		//env.registerSyntax(Kquasisyntax)
		//env.registerSyntax(Ksyntax)
		//env.registerSyntax(KsyntaxZKcase)
		fallthrough
	case v == 5:
		env.registerSyntax(KdefineZKsyntax)
		//env.registerSyntax(KletZKsyntax)
		//env.registerSyntax(KletrecZKsyntax)
		env.registerSyntax(KsyntaxZKrules)
		fallthrough
	case v <= 4:
		//env.registerSyntax(Kand)
		env.registerSyntax(Kbegin)
		//env.registerSyntax(Kcase)
		//env.registerSyntax(Kcond)
		env.registerSyntax(Kdefine)
		//env.registerSyntax(Kdo)
		env.registerSyntax(Kif)
		env.registerSyntax(Klambda)
		env.registerSyntax(Klet)
		env.registerSyntax(KletZH)
		//env.registerSyntax(Kletrec)
		//env.registerSyntax(Kor)
		env.registerSyntax(Kquasiquote)
		env.registerSyntax(Kquote)
		env.registerSyntax(KsetZA)
		env.registerSyntax(Kunquote)
		env.registerSyntax(KunquoteZKsplicing)
		fallthrough
	case v == 0:
		return env
	default:
		panic(newSyntaxError("null-environment unknown version"))
	}
	return env
}

func DnullZS(a Any) Any {
	return SBool(IsNull(unlist1(a)))
}

func DnumZH(a Any) Any {
	x, y := unlist2Number(a)
	return x.Mul(y)
}

func DnumZI(a Any) Any {
	x, y := unlist2Number(a)
	return x.Add(y)
}

func DnumZK(a Any) Any {
	x, y := unlist2Number(a)
	return x.Sub(y)
}

func DnumZM(a Any) Any {
	x, y := unlist2Number(a)
	return x.Div(y)
}

func DnumZQ(a Any) Any {
	return SBool(RealNum.Cmp(unifyRealNum(a)) == 0)
}

func DnumZP(a Any) Any {
	return SBool(RealNum.Cmp(unifyRealNum(a)) == -1)
}

func DnumZR(a Any) Any {
	return SBool(RealNum.Cmp(unifyRealNum(a)) == 1)
}

func DnumZPZQ(a Any) Any {
	return SBool(RealNum.Cmp(unifyRealNum(a)) <= 0)
}

func DnumZRZQ(a Any) Any {
	return SBool(RealNum.Cmp(unifyRealNum(a)) >= 0)
}

// (number->string z)
// (number->string z radix)
// (number->string z radix precision)
func DnumberZKZRstring(a Any) Any {
	// TODO other arguments
	z := unlist1(a)
	return ToString(z.(fmt.Stringer).String())
}

func DnumberZS(a Any) Any {
	return SBool(IsNumber(unlist1(a)))
}

func Dnumerator(a Any) Any {
	// TODO
	return list0()
}

// (open-binary-input-file filename)
func DopenZKbinaryZKinputZKfile(a Any) Any {
	return OpenBIFile(unlist1(a).(SString).GoString())
}

// (open-binary-output-file filename)
func DopenZKbinaryZKoutputZKfile(a Any) Any {
	return OpenBOFile(unlist1(a).(SString).GoString())
}

// (open-input-bytevector b)
func DopenZKinputZKbytevector(a Any) Any {
	return &SBytePort{it: unlist1(a).(SBinary), code: PortTypeCodeByteIn}
}

// (open-input-file filename)
func DopenZKinputZKfile(a Any) Any {
	return OpenTIFile(unlist1(a).(SString).GoString())
}

// (open-input-string s)
func DopenZKinputZKstring(a Any) Any {
	return &SCharPort{it: unlist1(a).(SString), code: PortTypeCodeCharIn}
}

// (open-output-bytevector)
func DopenZKoutputZKbytevector(a Any) Any {
	return &SBytePort{it: NewBinary([]byte{}), code: PortTypeCodeByteOut}
}

// (open-output-file filename)
func DopenZKoutputZKfile(a Any) Any {
	return OpenTOFile(unlist1(a).(SString).GoString())
}

// (open-output-string)
func DopenZKoutputZKstring(a Any) Any {
	return &SCharPort{it: NewString([]rune{}), code: PortTypeCodeCharOut}
}

func DoutputZKportZS(a Any) Any {
	return SBool(IsOutputPort(unlist1(a)))
}

func DpairZS(a Any) Any {
	return SBool(IsPair(unlist1(a)))
}

func DpeekZKchar(a Any) Any {
	port := unlist1(a)
	r, _, err := port.(RunePeeker).PeekRune()
	if err != nil {
		panic(err)
	}
	return Sint64(r)
}

func DpeekZKu8(a Any) Any {
	port := unlist1(a)
	b, err := port.(BytePeeker).PeekByte()
	if err != nil {
		panic(err)
	}
	return Sint64(b)
}

func DportZKopenZS(a Any) Any {
	return Dnot(list1(SBool(unlist1(a).(Closer).Closed())))
}

func DportZS(a Any) Any {
	return SBool(IsPort(unlist1(a)))
}

func DpositiveZS(a Any) Any {
	return SBool(unlist1(a).(RealNum).Cmp(Sfloat64(0)) == 1)
}

func DprocedureZS(a Any) Any {
	return SBool(IsProcedure(unlist1(a)))
}

func Draise(a Any) Any {
	panic(unlist1(a))
	return Void()
}

func DraiseZKcontinuable(a Any) Any {
	panic(unlist1(a))
	return Void()
}

func DrationalZS(a Any) Any {
	return SBool(IsRational(unlist1(a)))
}

func Drationalize(a Any) Any {
	return Void()
}

func DrationalizeZKZRexact(a Any) Any {
	return Void()
}

func Dread(a Any) Any {
	//port := unlist0O(a, DcurrentZKinputZKport(list0()))

	//// slurp
	//bufp := bufio.NewReaderSize(port.(io.Reader), 16777216)
	//input, err := bufp.ReadString(eof)
	//if err != nil {
	//	if err != io.EOF {
	//		panic(err)
	//	}
	//	err = nil
	//}

	return Void()
}

func DreadZKbytevector(a Any) Any {
	l, port := unlist1O(a, DcurrentZKinputZKport(list0()))
	length := ToFixnum(l)
	buffer := make([]byte, length)
	_, err := port.(BIPort).Read(buffer)
	if err == io.EOF {
		return DeofZKobject(list0())
	}
	return NewBinary(buffer)
}

func DreadZKbytevectorZA(a Any) Any {
	v, s, e, port := unlist3O(a, DcurrentZKinputZKport(list0()))
	buffer := v.(SBinary)[ToFixnum(s):ToFixnum(e)]
	_, err := port.(BIPort).Read(buffer)
	if err == io.EOF {
		return DeofZKobject(list0())
	}
	return list0()
}

func DreadZKchar(a Any) Any {
	port := unlist1(a)
	if !IsTextualPort(port) {
		panic(newTypeError("read-char expected textual-port"))
	}
	if !IsInputPort(port) {
		panic(newTypeError("read-char expected input-port"))
	}
	r, _, err := port.(TIPort).ReadRune()
	if err != nil {
		if err != io.EOF {
			panic(err)
		}
		return DeofZKobject(list0())
	}
	return SChar(r)
}

func DreadZKline(a Any) Any {
	port := unlist0O(a, DcurrentZKinputZKport(list0()))
	buf, err := port.(TIPort).ReadLine()
	if err != nil {
		if err != io.EOF {
			panic(err)
		}
		return DeofZKobject(list0())
	}
	return NewString(buf)
}

func DreadZKu8(a Any) Any {
	port := unlist1(a)
	if !IsBinaryPort(port) {
		panic(newTypeError("read-u8 expected binary-port"))
	}
	if !IsInputPort(port) {
		panic(newTypeError("read-u8 expected input-port"))
	}
	b, err := port.(BIPort).ReadByte()
	if err != nil {
		if err != io.EOF {
			panic(err)
		}
		return DeofZKobject(list0())
	}
	return Sint64(b)
}

func DrealZKpart(a Any) Any {
	return unlist1(a).(ComplexNum).Real()
}

func DrealZS(a Any) Any {
	return SBool(IsReal(unlist1(a)))
}

func Dround(a Any) Any {
	return unlist1(a).(RealNum).RTE()
}

// (set-car! ls expr)
func DsetZKcarZA(a Any) Any {
	ls, value := unlist2(a)
	ls.(*List).car = value
	return Void()
}

// (set-cdr! ls expr)
func DsetZKcdrZA(a Any) Any {
	ls, value := unlist2(a)
	ls.(*List).cdr = value
	return Void()
}

// (scheme-primitive-environment version)
func DschemeZKprimitiveZKenvironment(a Any) Any {
	env := DnullZKenvironment(a).(*Env).Extend()

	switch v := ToFixnum(unlist1(a)); {
	case v == 0:
		return env

	case v == 'D': // droscheme extensions

		env.register(DbytevectorZKZRu8ZKlist)
		env.register(DbytevectorZKZRu8ZKvector)
		env.register(DbytevectorZKu8)
		env.register(DcallZKwithZKescapeZKcontinuation)
		env.register(DhashtableZKZRlist)
		env.register(DhashtableZKZRvector)
		env.register(DhashtableZKvalues)
		env.register(DmakeZKeqZKhashtable)
		env.register(DmakeZKeqvZKhashtable)
		env.register(DmakeZKhashtable)
		env.register(DdroschemeZKbranch)
		env.register(DdroschemeZKrootZKpath)
		env.register(DdroschemeZKwait)
		env.register(DeofZKobject)
		env.register(DevalZKliteral)
		env.register(DlistZH)
		env.register(DlistZHZKZRvector)
		env.register(DnumZH)
		env.register(DnumZI)
		env.register(DnumZK)
		env.register(DnumZM)
		env.register(DnumZP)
		env.register(DnumZPZQ)
		env.register(DnumZQ)
		env.register(DnumZR)
		env.register(DnumZRZQ)
		env.register(DnumberZKtypeZKof)
		env.register(Dsign)
		env.register(DstandardZKerrorZKport)
		env.register(DstandardZKinputZKport)
		env.register(DstandardZKoutputZKport)
		env.register(DtypeZKof)
		env.register(Du8ZKlistZKZRbytevector)
		env.register(Du8ZKvectorZKZRbytevector)
		env.register(DvectorZKZRlistZH)
		env.register(Dvoid)

		fallthrough
	case v == 7: // R7RS

		env.register(DbinaryZKportZS)
		env.register(DbytevectorZKcopy)
		env.register(DbytevectorZKcopyZA)
		env.register(DbytevectorZKcopyZKpartial)
		env.register(DbytevectorZKcopyZKpartialZA)
		env.register(DbytevectorZKlength)
		env.register(DbytevectorZKu8ZKref)
		env.register(DbytevectorZKu8ZKsetZA)
		env.register(DbytevectorZS)
		env.register(DcallZKwithZKport)
		env.register(DcloseZKport)
		env.register(DcurrentZKerrorZKport)
		env.register(DcurrentZKjiffy)
		env.register(DcurrentZKsecond)
		env.register(DemptyZS)
		env.register(Derror)
		env.register(DerrorZKobjectZKirritants)
		env.register(DerrorZKobjectZKmessage)
		env.register(DerrorZKobjectZS)
		env.register(DfoldZKleft)
		env.register(DfoldZKright)
		env.register(DflushZKoutputZKport)
		env.register(DgetZKoutputZKbytevector)
		env.register(DgetZKoutputZKstring)
		env.register(Dhash)
		env.register(DmakeZM)
		env.register(DmakeZKbytevector)
		env.register(DmakeZKlist)
		env.register(DmakeZKparameter)
		env.register(DopenZKinputZKbytevector)
		env.register(DopenZKinputZKstring)
		env.register(DopenZKoutputZKbytevector)
		env.register(DopenZKoutputZKstring)
		env.register(DpeekZKu8)
		env.register(DportZKopenZS)
		env.register(Draise)
		env.register(DraiseZKcontinuable)
		env.register(DreadZKbytevector)
		env.register(DreadZKbytevectorZA)
		env.register(DreadZKline)
		env.register(DreadZKu8)
		//env.register(DstringZKmap)
		env.register(DtextualZKportZS)
		env.register(DtypeZQZS)
		env.register(Du8ZKreadyZS)
		env.register(Dutf8ZKZRstring)
		env.register(DwithZKexceptionZKhandler)
		env.register(DwriteZKbytevector)
		env.register(DwriteZKpartialZKbytevector)
		env.register(DwriteZKu8)

		fallthrough
	case v == 60: // R6RS libraries

		env.register(DhashtableZKclearZA)
		env.register(DhashtableZKcontainsZS)
		env.register(DhashtableZKcopy)
		env.register(DhashtableZKdeleteZA)
		env.register(DhashtableZKentries)
		env.register(DhashtableZKequivalenceZKfunction)
		env.register(DhashtableZKhashZKfunction)
		env.register(DhashtableZKkeys)
		env.register(DhashtableZKmutableZS)
		env.register(DhashtableZKref)
		env.register(DhashtableZKsetZA)
		env.register(DhashtableZKsize)
		env.register(DhashtableZKupdateZA)

		fallthrough
	case v == 6: // R6RS core
		//assert
		//library
		//env.register(DassertionZKviolation)
		env.register(DbooleanZQZS)
		//env.register(DconditionZS)
		//env.register(Ddiv)
		//env.register(Ddiv0)
		//env.register(DdivZKandZKmod)
		//env.register(Ddiv0ZKandZKmod0)
		//env.register(Derror)
		//env.register(Dexact) = inexact->exact
		//env.register(DexactZKintegerZKsqrt)
		//env.register(DfiniteZS)
		//env.register(Dinexact) = exact->inexact
		//env.register(DinfiniteZS)
		//env.register(DintegerZKvaluedZS) = integer?
		//env.register(Dmod)
		//env.register(Dmod0)
		//env.register(DnanZS)
		//env.register(Draise)
		//env.register(DraiseZKcontinuable)
		//env.register(DrationalZKvaluedZS) = rational?
		//env.register(DrealZKvaluedZS) = real?
		//env.register(DstringZKforZKeach)
		//env.register(DstringZKmap)
		//env.register(DsymbolZQZS)
		//env.register(Dthrow)
		env.register(DvectorZKforZKeach)
		env.register(DvectorZKmap)
		env.register(DvectorZKZRstring)
		env.register(DvectorZKcopy)
		env.register(DstringZKZRutf8)
		env.register(DstringZKZRvector)
		env.register(DlistZKcopy)

		fallthrough
	case v == 5: // R5RS

		env.register(DcallZKwithZKvalues)
		env.register(DdynamicZKwind)
		env.register(Deval)
		env.register(DinteractionZKenvironment)
		env.register(DnullZKenvironment)
		env.register(DportZS)
		env.register(DschemeZKreportZKenvironment)
		env.register(Dvalues)

		fallthrough
	case v <= 4: // R4RS

		env.register(DlistZS) // looks like this replaced DlastZKpair
		env.register(DpeekZKchar)
		env.register(Dstring)

		fallthrough
	case v == 3: // R3RS, also introduced delay/force

		env.register(DbooleanZS)
		env.register(Ddenominator)
		env.register(DnumberZKZRstring)
		env.register(Dnumerator)
		env.register(DprocedureZS)
		env.register(DstringZKZRnumber)

		fallthrough
	case v == 2: // R2RS

		//env.register(DZH)   // derived
		//env.register(DZI)	  // derived
		//env.register(DZK)	  // derived
		//env.register(DZM)	  // derived
		//env.register(DZP)	  // derived
		//env.register(DZPZQ) // derived
		//env.register(DZQ)	  // derived
		//env.register(DZR)	  // derived
		//env.register(DZRZQ) // derived
		//env.register(Dabs)  // derived
		env.register(Dacos)
		env.register(Dangle)
		//env.register(Dappend) // derived
		//env.register(DappendZA) // R2RS only
		env.register(Dapply) // derived
		env.register(Dasin)
		//env.register(Dassoc)  // derived
		//env.register(Dassq)   // derived
		//env.register(Dassv)   // derived
		env.register(Datan)
		env.register(DcallZKwithZKcurrentZKcontinuation)
		//env.register(DcallZKwithZKinputZKfile)  // R2RS, R4RS, R5RS
		//env.register(DcallZKwithZKoutputZKfile) // R2RS, R4RS, R5RS
		env.register(Dcar)
		//env.register(Dcaar)   // derived
		//env.register(Dcadr)   // derived
		//env.register(Dcaaar)  // derived
		//env.register(Dcaadr)  // derived
		//env.register(Dcadar)  // derived
		//env.register(Dcaddr)  // derived
		//env.register(Dcaaaar) // derived
		//env.register(Dcaaadr) // derived
		//env.register(Dcaadar) // derived
		//env.register(Dcaaddr) // derived
		//env.register(Dcadaar) // derived
		//env.register(Dcadadr) // derived
		//env.register(Dcaddar) // derived
		//env.register(Dcadddr) // derived
		//env.register(Dcdaaar) // derived
		//env.register(Dcdaadr) // derived
		//env.register(Dcdadar) // derived
		//env.register(Dcdaddr) // derived
		//env.register(Dcddaar) // derived
		//env.register(Dcddadr) // derived
		//env.register(Dcdddar) // derived
		//env.register(Dcddddr) // derived
		//env.register(Dcdaar)  // derived
		//env.register(Dcdadr)  // derived
		//env.register(Dcddar)  // derived
		//env.register(Dcdddr)  // derived
		//env.register(Dcdar)   // derived
		//env.register(Dcddr)   // derived
		env.register(Dcdr)
		//env.register(Dceiling)
		env.register(DcharZKZRinteger)
		//env.register(DcharZKalphabeticZS)	  // R2RS, case
		//env.register(DcharZKciZPZQZS)       // R2RS, case
		//env.register(DcharZKciZPZS)         // R2RS, case
		//env.register(DcharZKciZQZS)         // R2RS, case
		//env.register(DcharZKciZRZQZS)       // R2RS, case
		//env.register(DcharZKciZRZS)         // R2RS, case
		//env.register(DcharZKdowncase)       // R2RS, case
		//env.register(DcharZKlowerZKcaseZS)  // R2RS, case
		//env.register(DcharZKnumericZS)	  // R2RS, case
		//env.register(DcharZKupcase)         // R2RS, case
		//env.register(DcharZKupperZKcaseZS)  // R2RS, case
		//env.register(DcharZKwhitespaceZS)	  // R2RS, case
		env.register(DcharZKreadyZS)
		//env.register(DcharZPZQZS) // derived
		//env.register(DcharZPZS)	// derived
		//env.register(DcharZQZS)	// derived
		//env.register(DcharZRZQZS)	// derived
		//env.register(DcharZRZS)   // derived
		env.register(DcharZS)
		env.register(DcloseZKinputZKport)
		env.register(DcloseZKoutputZKport)
		env.register(DcomplexZS)
		env.register(Dcons)
		env.register(Dcos)
		env.register(DcurrentZKinputZKport)     // R2RS, R4RS, R5RS
		env.register(DcurrentZKoutputZKport)    // R2RS, R4RS, R5RS
		env.register(Ddisplay)
		env.register(DeofZKobjectZS)
		env.register(DeqZS)
		env.register(DequalZS)
		env.register(DeqvZS)
		//env.register(DevenZS) // derived
		env.register(DexactZKZRinexact)
		env.register(DexactZS)
		env.register(Dexp)
		env.register(Dexpt)
		env.register(Dfloor)
		//env.register(DforZKeach)
		//env.register(Dgcd) // derived
		env.register(DimagZKpart)
		env.register(DinexactZKZRexact)
		env.register(DinexactZS)
		env.register(DinputZKportZS)
		env.register(DintegerZKZRchar)
		env.register(DintegerZS)
		env.register(DlastZKpair) // R2RS only
		//env.register(Dlcm) // derived
		env.register(Dlength)
		env.register(Dlist)
		env.register(DlistZKZRstring)
		env.register(DlistZKZRvector)
		env.register(DlistZKref)
		env.register(DlistZKtail)
		env.register(Dload)
		env.register(Dlog)
		env.register(Dmagnitude)
		env.register(DmakeZKpolar)
		env.register(DmakeZKrectangular)
		env.register(DmakeZKstring)
		env.register(DmakeZKvector)
		//env.register(Dmap)
		//env.register(Dmax)	// derived
		//env.register(Dmember) // derived
		//env.register(Dmemq)	// derived
		//env.register(Dmemv)	// derived
		//env.register(Dmin)	// derived
		//env.register(Dmodulo)	// derived
		env.register(DnegativeZS)
		env.register(Dnewline)
		env.register(Dnot)
		env.register(DnullZS)
		env.register(DnumberZS)
		//env.register(DobjectZKhash)   // R2RS only
		//env.register(DobjectZKunhash) // R2RS only
		//env.register(DoddZS) // derived
		//env.register(DopenZKinputZKfile)  // R2RS, R4RS, R5RS
		//env.register(DopenZKoutputZKfile) // R2RS, R4RS, R5RS
		env.register(DoutputZKportZS)
		env.register(DpairZS)
		env.register(DpositiveZS)
		//env.register(Dquotient)  // derived
		env.register(DrationalZS)
		env.register(Drationalize)
		env.register(Dread)
		env.register(DreadZKchar)
		env.register(DrealZKpart)
		env.register(DrealZS)
		//env.register(Dremainder) // derived
		//env.register(Dreverse) // derived
		env.register(Dround)
		env.register(DsetZKcarZA) // mutable
		env.register(DsetZKcdrZA) // mutable
		env.register(Dsin)
		env.register(Dsqrt)
		env.register(DstringZKZRlist)
		env.register(DstringZKZRsymbol)
		env.register(DstringZKappend)
		//env.register(DstringZKciZPZQZS) // R2RS, case
		//env.register(DstringZKciZPZS)   // R2RS, case
		//env.register(DstringZKciZQZS)   // R2RS, case
		//env.register(DstringZKciZRZQZS) // R2RS, case
		//env.register(DstringZKciZRZS)   // R2RS, case
		env.register(DstringZKcopy)
		env.register(DstringZKfillZA)
		env.register(DstringZKlength)
		env.register(DstringZKref)
		env.register(DstringZKsetZA)
		//env.register(DstringZPZQZS) // derived
		//env.register(DstringZPZS)	  // derived
		//env.register(DstringZQZS)	  // derived
		//env.register(DstringZRZQZS) // derived
		//env.register(DstringZRZS)   // derived
		env.register(DstringZS)
		env.register(Dsubstring)
		//env.register(DsubstringZKfillZA)        // R2RS only
		//env.register(DsubstringZKmoveZKleftZA)  // R2RS only
		//env.register(DsubstringZKmoveZKrightZA) // R2RS only
		env.register(DsymbolZKZRstring)
		env.register(DsymbolZS)
		env.register(Dtan)
		//env.register(DtranscriptZKoff) // R2RS, R3RS, R4RS, R5RS // deprecated
		//env.register(DtranscriptZKon)  // R2RS, R3RS, R4RS, R5RS // deprecated
		//env.register(Dtruncate)
		env.register(Dvector)
		env.register(DvectorZKZRlist)
		env.register(DvectorZKfillZA)
		env.register(DvectorZKlength)
		env.register(DvectorZKref)
		env.register(DvectorZKsetZA)
		env.register(DvectorZS)
		//env.register(DwithZKinputZKfromZKfile)  // R2RS, R4RS, R5RS
		//env.register(DwithZKoutputZKtoZKfile)   // R2RS, R4RS, R5RS
		env.register(Dwrite)
		env.register(DwriteZKchar)
		//env.register(DzeroZS) // derived

		// syntax
		//Kquote
		//Klambda
		//Kif
		//Kcond
		//Kcase
		//Kand
		//Kor
		//Klet
		//KletZH
		//Kletrec
		//Krec -- derived
		//KnamedZKlambda -- derived
		//Kbegin
		//kdo -- derived

		fallthrough
	case v == 1:
		//labels
		//aset'
		//fluidbind
		//fluid
		//fluidset'
		//catch
		//block
		//let
		//do
		//iterate
		//test
		//cond
		//or
		//and
		//amapcar
		//amaplist
		//amapc
		//prog
		//schmac
		//macro
		//progp
		//enclose
		//create!process
		//start!process
		//stop!process
		//terminate
	default:
		panic(newSyntaxError("scheme-report-environment unknown version"))
	}
	return env
}

// (scheme-report-environment version)
func DschemeZKreportZKenvironment(a Any) Any {
	var src string = GetRootPath() + "/src/"
	//fmt.Printf("src=%s\n", src)
	env := DschemeZKprimitiveZKenvironment(a).(*Env).Extend()
	switch ToFixnum(unlist1(a)) {
	case 0:
		panic(src)
		return env
	case 7:
		// load (scheme base)
		Dload(list2(ToString(src + "scheme.base.7.ss"), env))
	case 6:
		// load (rnrs base)
		Dload(list2(ToString(src + "scheme.base.6.ss"), env))
	case 5:
		// load (rnrs r5rs)
		Dload(list2(ToString(src + "scheme.base.5.ss"), env))
	case 'D':
		fallthrough
	default:
		// load (ds base)
		Dload(list2(ToString(src + "ds.base.ss"), env))
	}
	return env
}

func DshowZKlist(a Any) Any {
	if DlistZS(a).(SBool) {
		v := DlistZKZRvector(a)
		s := fmt.Sprintf("%s", v)
		return ToString(s[1:])
	}

	o := unlist1(a).(*List)

	return ToString(fmt.Sprintf("(%s . %s)", o.car, o.cdr))
}

func DshowZKvector(a Any) Any {
	o := unlist1(a).(SVector)

	if len(o) == 0 {
		return ToString("#()")
	}

	var ret string = ""
	for i := 0; i < len(o); i++ {
		ret += fmt.Sprintf(" %s", o[i])
	}

	return ToString(fmt.Sprintf("#(%s)", ret[1:]))
}

func DsimplestZKrationalZKZRexact(a Any) Any {
	// see also <http://trac.sacrideo.us/wg/wiki/RationalizeDefinition>
	lo, hi := unlist2float64(a)
	var g, n, d float64 = 0.0, 0.0, 1.0
	avg := (lo + hi)/2.0
	if avg > 1.0 {
		//for m = 0.0; lo > m || m > hi; m = n/d {
		d++
		for n = d*lo; n <= d*hi; n++ {
			g = n/d
			if lo > g || g > hi {
				return NewRational64(int64(n), int64(d))
			}
		}
		//}
	} else {
		for d = n/hi; d <= n/lo; n++ {
			g = n/d
			if lo > g || g > hi {
				return NewRational64(int64(n), int64(d))
			}
		}
	}
	return Void()
}

func Dsign(a Any) Any {
	return Sint64(RealNum.Cmp(unifyRealNum(list2(unlist1(a), Sint64(0)))))
}

func Dsin(a Any) Any {
	return unlist1(a).(TrigNum).Sin()
}

func Dsqrt(a Any) Any {
	return Sfloat64(ToFlonum(unlist1(a))).Sqrt()
}

func DstandardZKerrorZKport(a Any) Any {
	return SFilePort{os.Stderr, "/dev/stderr", PortTypeCodeCharOut, false}
}

func DstandardZKinputZKport(a Any) Any {
	return SFilePort{os.Stdin, "/dev/stdin", PortTypeCodeCharIn, false}
}

func DstandardZKoutputZKport(a Any) Any {
	return SFilePort{os.Stdout, "/dev/stdout", PortTypeCodeCharOut, false}
}

func Dstring(a Any) Any {
	return DlistZKZRstring(list1(a))
}

func DstringZKZRlist(a Any) Any {
	return DvectorZKZRlist(list1(DstringZKZRvector(a)))
}

// (string->number string)
// (string->number string radix)
func DstringZKZRnumber(a Any) Any {
	s, rest := unlist1R(a)
	base = 10
	if IsPair(rest) {
		base = int(ToFixnum(unlist1(rest)))
	}
	lex := newLexer(s.(SString).GoString())
	lex.state = (*Lexer).lexNumber
	yyParse(lex)
	if parseErr != nil {
		panic(parseErr)
	}
	return parseValue
}

func DstringZKZRsymbol(a Any) Any {
	str := unlist1(a).(SString).GoString()
	return NewSymbol(str)
}

func DstringZKZRutf8(a Any) Any {
	return NewBinary([]byte(unlist1(a).(SString).GoString()))
}

func DstringZKZRvector(a Any) Any {
	sta := unlist1(a).(SString)
	str := []rune(sta)
	stv := make([]Any, len(str))
	for k, _ := range str {
		stv[k] = SChar(str[k])
	}
	return NewVector(stv)
}

func DstringZKappend(a Any) Any {
	if IsNull(a) {
		return ToString("")
	}
	b, rest := unlist1R(a)
	if IsPair(rest) {
		c, e := unlist1R(rest)
		d := append(b.(SString), c.(SString)...)
		return DstringZKappend(list1R(d, e))
	}
	return b
}

func DstringZKcopy(a Any) Any {
	first, rest := unlist1R(a)
	return DvectorZKZRstring(list1(DvectorZKcopy(list1R(DstringZKZRvector(list1(first)), rest))))
}

func DstringZKfillZA(a Any) Any {
	vac, opt := unlist1R(a)
	vec := vac.(SString)
	var inlen = len(vec)
	var fil Any = list0()
	var str Any = Sint64(0)
	var end Any = Sint64(inlen)
	if !IsNull(opt) {
		fil, opt = unlist1R(opt)
		if !IsNull(opt) {
			str, opt = unlist1R(opt)
			if !IsNull(opt) {
				end, opt = unlist1R(opt)
			}
		}
	}
	var s = int(ToFixnum(str))
	var e = int(ToFixnum(end))
	for i := s; i < e; i++ {
		vec[i] = rune(ToFixnum(fil))
	}
	return list0()
}

func DstringZKlength(a Any) Any {
	return Sint64(len(unlist1(a).(SString)))
}

func DstringZKref(a Any) Any {
	o, k := unlist2(a)
	return o.(SString).Ref(k)
}

func DstringZKsetZA(a Any) Any {
	o, k, v := unlist3(a)
	return o.(SString).Set(k, v)
}

func DstringZS(a Any) Any {
	return SBool(IsString(unlist1(a)))
}

func Dsubstring(a Any) Any {
	frs, s, e := unlist3(a)
	tos := DmakeZKstring(list1(DstringZKlength(list1(frs))))
	DsubstringZA(list3R(frs, s, e, list2(tos, Sint64(0))))
	return tos
}

// similar to (srfi 13) xsubstring but different
// (substring! from s e to at)
func DsubstringZA(a Any) Any {
	from, str, end, rest := unlist3R(a)
	to, ta := unlist2(rest)
	frv := from.(SString)
	tov := to.(SString)
	var t = int(ToFixnum(ta))
	var s = int(ToFixnum(str))
	var e = int(ToFixnum(end))
	for i := s; i < e; i++ {
		tov[i + t - s] = frv[i]
	}
	return Void()
}

func DsymbolZKZRstring(a Any) Any {
	sym := unlist1(a).(SSymbol).name
	return ToString(sym)
}

func DsymbolZS(a Any) Any {
	return SBool(IsSymbol(unlist1(a)))
}

func DsyntaxZKerror(a Any) Any {
	// TODO
	return Derror(a)
}

func Dtan(a Any) Any {
	return unlist1(a).(TrigNum).Tan()
}

func DtextualZKportZS(a Any) Any {
	return SBool(IsTextualPort(unlist1(a)))
}

func DnumberZKtypeZKof(a Any) Any {
	return NewSymbol(numberTypeToString(unlist1(a).(BaseNum).GetNumberType()))
}

func DtypeZKof(a Any) Any {
	return NewSymbol(typeToString(unlist1(a).GetType()))
}

func DtypeZQZS(a Any) Any {
	b, c := unlist2(a)
	return SBool(b.GetType() == c.GetType())
}

// R6RS:u8-list->bytevector
func Du8ZKlistZKZRbytevector(a Any) Any {
	var vec = []byte{}
	for cur := unlist1(a); IsPair(cur); cur = cur.(*List).cdr {
		num := cur.(*List).car
		if !IsByte(num) {
			panic("TypeError: expected byte")
		}
		vec = append(vec, ToByte(num))
	}
	return NewBinary(vec)
}

func Du8ZKvectorZKZRbytevector(a Any) Any {
	return Du8ZKlistZKZRbytevector(list1(DvectorZKZRlist(a)))
}

func Du8ZKreadyZS(a Any) Any {
	port := unlist0O(a, DcurrentZKinputZKport(list0()))
	_, err := port.(BytePeeker).PeekByte()
	if err != nil {
		return SBool(false)
	}
	return SBool(true)
}

func Dutf8ZKZRstring(a Any) Any {
	return NewString([]rune(string(unlist1(a).(SBinary))))
}

func Dvalues(a Any) Any {
	return listToValues(a)
}

func Dvector(a Any) Any {
	return DlistZKZRvector(list1(a))
}

// (vector->list v)
func DvectorZKZRlist(a Any) Any {
	vec := unlist1(a).(SVector)
	if len(vec) == 0 {
		return list0()
	}
	return list1R(vec[0], DvectorZKZRlist(list1(NewVector(vec[1:]))))
}

// (vector->list* v)
func DvectorZKZRlistZH(a Any) Any {
	vec := unlist1(a).(SVector)
	switch len(vec) {
	case 0:
		return list0()
	case 1:
		return vec[0]
	case 2:
		return list1R(vec[0], vec[1])
	}
	return list1R(vec[0], DvectorZKZRlistZH(list1(NewVector(vec[1:]))))
}

func DvectorZKZRstring(a Any) Any {
	vec := unlist1(a).(SVector)
	vev := make([]rune, len(vec))
	for k, _ := range vec {
		vev[k] = rune(ToFixnum(vec[k]))
	}
	return NewString(vev)
}

// (vector-copy vector)
// (vector-copy vector start)
// (vector-copy vector start end)
// (vector-copy vector start end fill)
func DvectorZKcopy(a Any) Any {
	vac, opt := unlist1R(a)
	vec := vac.(SVector)
	var inlen = len(vec)
	var str Any = Sint64(0)
	var end Any = Sint64(inlen)
	var fil Any = list0()
	if !IsNull(opt) {
		str, opt = unlist1R(opt)
		if !IsNull(opt) {
			end, opt = unlist1R(opt)
			if !IsNull(opt) {
				fil, opt = unlist1R(opt)
			}
		}
	}
	var s = int(ToFixnum(str))
	var e = int(ToFixnum(end))
	var outlen = e - s
	var ret = DmakeZKvector(list2(Sint64(outlen), fil)).(SVector)
	if e > inlen {
		// we need copy from start to inlen
		copy(ret[0:inlen - s], vec[s:inlen])
	} else {
		// we need copy from start to end
		copy(ret[0:outlen - s], vec[s:outlen])
	}
	return ret
}

// (vector-fill! vector fill)
// (vector-fill! vector fill start end)
func DvectorZKfillZA(a Any) Any {
	vac, opt := unlist1R(a)
	vec := vac.(SVector)
	var inlen = len(vec)
	var fil Any = list0()
	var str Any = Sint64(0)
	var end Any = Sint64(inlen)
	if !IsNull(opt) {
		fil, opt = unlist1R(opt)
		if !IsNull(opt) {
			str, opt = unlist1R(opt)
			if !IsNull(opt) {
				end, opt = unlist1R(opt)
			}
		}
	}
	var s = int(ToFixnum(str))
	var e = int(ToFixnum(end))
	for i := s; i < e; i++ {
		vec[i] = fil
	}
	return list0()
}

func DvectorZKforZKeach(a Any) Any {
	return list0()
}

func DvectorZKlength(a Any) Any {
	return Sint64(len(unlist1(a).(SVector)))
}

func DvectorZKmap(a Any) Any {
	return list0()
}

func DvectorZKref(a Any) Any {
	o, k := unlist2(a)
	return o.(SVector).Ref(k)
}

func DvectorZKsetZA(a Any) Any {
	o, k, v := unlist3(a)
	return o.(SVector).Set(k, v)
}

func DvectorZS(a Any) Any {
	return SBool(IsVector(unlist1(a)))
}

func Dvoid(a Any) Any {
	return SVoid{}
}

func DwithZKexceptionZKhandler(a Any) Any {
	return list0()
}

func Dwrite(a Any) Any {
	obj, port := unlist1O(a, DcurrentZKoutputZKport(list0()))
	s := fmt.Sprintf("%s", obj)
	port.(OPort).Write([]byte(s))
	return Void()
}

func DwriteZKbytevector(a Any) Any {
	bv, port := unlist1O(a, DcurrentZKoutputZKport(list0()))
	if !IsBinary(bv) {
		panic(newTypeError("write-bytevector expected bytevector"))
	}
	//if !IsBinaryPort(port) {
	//	panic(newTypeError("write-bytevector expected binary-port"))
	//}
	if !IsOutputPort(port) {
		panic(newTypeError("write-bytevector expected output-port"))
	}
	_, err := port.(BOPort).Write([]byte(bv.(SBinary)))
	if err != nil {
		panic(err)
	}
	return Void()
}

func DwriteZKchar(a Any) Any {
	ch, port := unlist1O(a, DcurrentZKoutputZKport(list0()))
	if !IsChar(ch) {
		panic(newTypeError("write-char expected char"))
	}
	//if !IsTextualPort(port) {
	//	panic(newTypeError("write-char expected textual-port"))
	//}
	if !IsOutputPort(port) {
		panic(newTypeError("write-char expected output-port"))
	}
	_, err := port.(TOPort).WriteRunes([]rune{rune(ch.(SChar))})
	if err != nil {
		panic(err)
	}
	return Void()
}

func DwriteZKpartialZKbytevector(a Any) Any {
	return list0()
}

func DwriteZKu8(a Any) Any {
	u8, port := unlist1O(a, DcurrentZKoutputZKport(list0()))
	if !IsByte(u8) {
		panic(newTypeError("write-u8 expected byte"))
	}
	//if !IsBinaryPort(port) {
	//	panic(newTypeError("write-u8 expected binary-port"))
	//}
	if !IsOutputPort(port) {
		panic(newTypeError("write-u8 expected output-port"))
	}
	_, err := port.(BOPort).Write([]byte{byte(ToFixnum(u8))})
	if err != nil {
		panic(err)
	}
	return Void()
}
