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
	"os"
)

/*
 * Procedures of the form K<mangled name> recieve arguments as
 *   kw = <keyword>
 *   st = (<arg1> ... <argn>)
 * Procedures of the form D<mangled name> recieve arguments as
 *   a = (<arg1> ... <argn>)
 */

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
	//cenv := env.Extend()
	////names := DlistZKref(list2(st, Sint64(0))) // TODO
	//exprt := DlistZKref(list2(st, Sint64(1)))
	////imprt := DlistZKref(list2(st, Sint64(2))) // TODO
	//begin := DlistZKref(list2(st, Sint64(3)))
	//Deval(list2(begin, cenv))
	//exprtIds := exprt.(SPair).cdr.(SPair).ToVector()
	//for _, symbol := range exprtIds.(SVector).it {
	//	id := symbol.(SSymbol).String()
	//	env.bound[id] = cenv.bound[id]
	//}
	return Void()
}

func KdefineZKsyntax(kw, st Any, env *Env) Any {
	return Void()
}

func Kdo(kw, st Any, env *Env) Any {
	return Void()
}

// (dump-environment) -- for debug only
func KdumpZKenvironment(kw, st Any, env *Env) Any {
	env.dump()
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
	return Void()
}

// (let-values)
func KletZKvalues(kw, st Any, env *Env) Any {
	return Void()
}

// (letrec-values)
func KletrecZKvalues(kw, st Any, env *Env) Any {
	return Void()
}

// (library (module name) ...) // R6RS
func Klibrary(kw, st Any, env *Env) Any {
	return Void()
}

// (quote expr)
func Kquote(kw, st Any, env *Env) Any {
	return unlist1(st)
}

// (set! var expr)
func KsetZA(kw, st Any, env *Env) Any {
	symbol, value := unlist2(st)
	return env.Set(symbol, value)
}

func KsyntaxZKcase(kw, st Any, env *Env) Any {
	expr, literals, body := unlist2R(st)
	return SCaseSyntax{expr: expr, lits: literals, body: body, env: env}
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
	x := unlist1(a)
	return x.(TrigNum).ArcCos()
}

func Dangle(a Any) Any {
	return unlist1(a).(ComplexNum).Angle()
}

//func Dappend(a Any) Any {
//	return list0()
//}

// (apply proc arg1 ... restargs)
/*
(define (cons* . rest)
  (append (most rest) (last rest)))
 * (define (apply proc . rest)
 *     (define (apply-1 args) (proc . args))
 *     (apply-1 (cons* . rest)))
 */
func Dapply(a Any) Any {
	//proc, rest := unlist2(a)
	//return proc.(Applier).Apply(rest)
	proc, rest := unlist1R(a)
	return proc.(Applier).Apply(DlistZH(rest))
}

func Dasin(a Any) Any {
	x := unlist1(a)
	return x.(TrigNum).ArcSin()
}

func Datan(a Any) Any {
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
	return list0()
}

func DbytevectorZKcopyZA(a Any) Any {
	return list0()
}

func DbytevectorZKcopyZKpartial(a Any) Any {
	return list0()
}

func DbytevectorZKcopyZKpartialZA(a Any) Any {
	return list0()
}

func DbytevectorZKlength(a Any) Any {
	return Sint64(len(unlist1(a).(SBinary)))
}

func DbytevectorZKu8ZKref(a Any) Any {
	o, k := unlist2(a)
	return Sint64(o.(SBinary)[ToFixnum(k)])
}

func DbytevectorZKu8ZKsetZA(a Any) Any {
	o, k, v := unlist3(a)
	o.(SBinary)[ToFixnum(k)] = byte(ToFixnum(v))
	return Void()
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

func DcallZKwithZKcurrentZKcontinuation(a Any) Any {
	return list0()
}

func DcallZKwithZKport(a Any) Any {
	return list0()
}

func DcallZKwithZKvalues(a Any) Any {
	return list0()
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

func Dceiling(a Any) Any {
	return list0()
}

func DcharZKZRinteger(a Any) Any {
	return Sint64(unlist1(a).(SChar))
}

func DcharZKreadyZS(a Any) Any {
	return list0()
}

func DcharZPZQZS(a Any) Any {
	return list0()
}

func DcharZPZS(a Any) Any {
	return list0()
}

func DcharZQZS(a Any) Any {
	b, c := unlist2(a)
	return SBool(Equal(b, c))
}

func DcharZRZQZS(a Any) Any {
	return list0()
}

func DcharZRZS(a Any) Any {
	return list0()
}

func DcharZS(a Any) Any {
	return SBool(IsChar(unlist1(a)))
}

func DcloseZKinputZKport(a Any) Any {
	return list0()
}

func DcloseZKoutputZKport(a Any) Any {
	return list0()
}

func DcloseZKport(a Any) Any {
	return list0()
}

func DcomplexZS(a Any) Any {
	return list0()
}

func Dcons(a Any) Any {
	return list1R(unlist2(a))
}

func Dcos(a Any) Any {
	x := unlist1(a)
	return x.(TrigNum).Cos()
}

func DcurrentZKerrorZKport(a Any) Any {
	return list0()
}

func DcurrentZKinputZKport(a Any) Any {
	return list0()
}

func DcurrentZKoutputZKport(a Any) Any {
	return list0()
}

func Ddenominator(a Any) Any {
	return list0()
}

func Ddisplay(a Any) Any {
	fmt.Printf("%s\n", unlist1(a).(SString))
	return Void()
}

func DdynamicZKwind(a Any) Any {
	return list0()
}

func DemptyZS(a Any) Any {
	return SBool(IsEmpty(unlist1(a)))
}

func DeofZKobjectZS(a Any) Any {
	return list0()
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
	return list0()
}

func DerrorZKobjectZKirritants(a Any) Any {
	return list0()
}

func DerrorZKobjectZKmessage(a Any) Any {
	return list0()
}

func DerrorZKobjectZS(a Any) Any {
	return list0()
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
	var n, ok = a.(Num)
	if !ok {
		return SBool(false)
	}
	var t = n.GetNumberType()
	return SBool(t&NumberTypeCodeInexact == 0)
}

func Dexp(a Any) Any {
	x := unlist1(a)
	return x.(TrigNum).Exp()
}

func Dexpt(a Any) Any {
	x, y := unify2(a)
	return x.(TrigNum).Pow(y)
}

func Dfloor(a Any) Any {
	return list0()
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
	if Length(rest) > 1 {
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
	return list0()
}

func DforZKeach(a Any) Any {
	return list0()
}

func DgetZKoutputZKbytevector(a Any) Any {
	return list0()
}

func DgetZKoutputZKstring(a Any) Any {
	return list0()
}

func Dhash(a Any) Any {
	return Sint64(int64(Hash(unlist1(a))))
}

func DimagZKpart(a Any) Any {
	return unlist1(a).(ComplexNum).Imag()
}

func DinexactZKZRexact(a Any) Any {
	return list0()
}

func DinexactZS(a Any) Any {
	var n, ok = a.(Num)
	if !ok {
		return SBool(false)
	}
	var t = n.GetNumberType()
	return SBool(t&NumberTypeCodeInexact != 0)
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
	return Sint64(Length(unlist1(a)))
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
	// in mutable model we need to copy
	// in immutable model we don't
	return unlist1(a)
}

func DlistZKref(a Any) Any {
	list, ka := unlist2(a)
	k := ToFixnum(ka)
	for cur := list; IsPair(cur); k, cur = k-1, cur.(*List).cdr {
		if k == 0 {
			return cur.(*List).car
		}
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
	return SBool(IsList(unlist1(a)))
}

func Dload(a Any) Any {
	fs, opt := unlist1O(a, BuiltinEnv())
	filename := fs.(SString).GoString()
	env := opt.(*Env)
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
		if err.Error() != "EOF" {
			panic(err)
		}
		err = nil
	}

	// read
	value, err = Read("(begin "+input+"\n)") // hack
	if err != nil {
		if err.Error() != "EOF" {
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

func DmakeZKlist(a Any) Any {
	return DvectorZKZRlist(list1(DmakeZKvector(a)))
}

func DmakeZKparameter(a Any) Any {
	return list0()
}

func DmakeZKpolar(a Any) Any {
	mag, ang := unlist2(a)
	return NewComplexPolar(mag.(Num), ang.(Num))
}

func DmakeZKrectangular(a Any) Any {
	x, y := unlist2(a)
	return NewComplex(x.(Num), y.(Num))
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

func Dmap(a Any) Any {
	return list0()
}

func DnegativeZS(a Any) Any {
	return SBool(unlist1(a).(RealNum).Cmp(Sfloat64(0)) == -1)
}

func Dnewline(a Any) Any {
	return list0()
}

func Dnot(a Any) Any {
	return SBool(!unlist1(a).(SBool))
}

// (null-environment version)
func DnullZKenvironment(a Any) Any {
	env := EmptyEnv()
	switch v := ToFixnum(unlist1(a)); {
	case v == 0:
		return env
	case v == 'D':
		env.registerSyntax(KcurrentZKenvironment)
		env.registerSyntax(KdumpZKenvironment)
		fallthrough
	case v == 7:
		env.registerSyntax(KdefineZKlibrary)
		//env.registerSyntax(KdefineZKrecordZKtype)
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
		//env.registerSyntax(Kquasiquote)
		env.registerSyntax(Kquote)
		env.registerSyntax(KsetZA)
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
	x, y := unify2(a)
	return x.Mul(y)
}

func DnumZI(a Any) Any {
	x, y := unify2(a)
	return x.Add(y)
}

func DnumZK(a Any) Any {
	x, y := unify2(a)
	return x.Sub(y)
}

func DnumZM(a Any) Any {
	x, y := unify2(a)
	return x.Div(y)
}

func DnumZQ(a Any) Any {
	ax, ay := unlist2(a)
	x := ax.(Num)
	y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return SBool(x.(RealNum).Cmp(y) == 0)
}

func DnumZP(a Any) Any {
	ax, ay := unlist2(a)
	x := ax.(Num)
	y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return SBool(x.(RealNum).Cmp(y) == -1)
}

func DnumZR(a Any) Any {
	ax, ay := unlist2(a)
	x := ax.(Num)
	y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return SBool(x.(RealNum).Cmp(y) == 1)
}

func DnumZPZQ(a Any) Any {
	ax, ay := unlist2(a)
	x := ax.(Num)
	y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return SBool(x.(RealNum).Cmp(y) <= 0)
}

func DnumZRZQ(a Any) Any {
	ax, ay := unlist2(a)
	x := ax.(Num)
	y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return SBool(x.(RealNum).Cmp(y) >= 0)
}

func DnumberZKZRstring(a Any) Any {
	return list0()
}

func DnumberZS(a Any) Any {
	_, ok := unlist1(a).(Num)
	return SBool(ok)
}

func Dnumerator(a Any) Any {
	return list0()
}

func DopenZKinputZKbytevector(a Any) Any {
	return list0()
}

func DopenZKinputZKstring(a Any) Any {
	return list0()
}

func DopenZKoutputZKbytevector(a Any) Any {
	return list0()
}

func DopenZKoutputZKstring(a Any) Any {
	return list0()
}

func DoutputZKportZS(a Any) Any {
	return SBool(IsOutputPort(unlist1(a)))
}

func DpairZS(a Any) Any {
	return SBool(IsPair(unlist1(a)))
}

func DpeekZKchar(a Any) Any {
	return list0()
}

func DpeekZKu8(a Any) Any {
	return list0()
}

func DportZKopenZS(a Any) Any {
	return list0()
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
	return list0()
}

func DraiseZKcontinuable(a Any) Any {
	return list0()
}

func DrationalZS(a Any) Any {
	return list0()
}

func Drationalize(a Any) Any {
	return list0()
}

func Dread(a Any) Any {
	return Void()
}

func DreadZKbytevector(a Any) Any {
	return list0()
}

func DreadZKbytevectorZA(a Any) Any {
	return list0()
}

func DreadZKchar(a Any) Any {
	port := unlist1(a)
	return port.(Port).Read()
}

func DreadZKline(a Any) Any {
	return list0()
}

func DreadZKu8(a Any) Any {
	return list0()
}

func DrealZKpart(a Any) Any {
	return unlist1(a).(ComplexNum).Real()
}

func DrealZS(a Any) Any {
	return list0()
}

func Dround(a Any) Any {
	return list0()
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
		env.register(DstringZKmap)
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

		//env.register(DZH)
		//env.register(DZI)
		//env.register(DZK)
		//env.register(DZM)
		//env.register(DZP)
		//env.register(DZPZQ)
		//env.register(DZQ)
		//env.register(DZR)
		//env.register(DZRZQ)
		//env.register(Dabs) // derived
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
		env.register(Dceiling)
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
		env.register(DcharZPZQZS)
		env.register(DcharZPZS)
		env.register(DcharZQZS)
		env.register(DcharZRZQZS)
		env.register(DcharZRZS)
		env.register(DcharZS)
		env.register(DcloseZKinputZKport)
		env.register(DcloseZKoutputZKport)
		env.register(DcomplexZS)
		env.register(Dcons)
		env.register(Dcos)
		//env.register(DcurrentZKinputZKport)     // R2RS, R4RS, R5RS
		//env.register(DcurrentZKoutputZKport)    // R2RS, R4RS, R5RS
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
		env.register(DforZKeach)
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
		env.register(Dmap)
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
		env.register(DstringZPZQZS)
		env.register(DstringZPZS)
		env.register(DstringZQZS)
		env.register(DstringZRZQZS)
		env.register(DstringZRZS)
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
		env.register(Dtruncate)
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
	env := DschemeZKprimitiveZKenvironment(a).(*Env).Extend()
	switch ToFixnum(unlist1(a)) {
	case 7:
		// load (scheme base)
	case 6:
		// load (rnrs base)
	default:
		// load (ds base)
	}
	return env
}

func Dsin(a Any) Any {
	x := unlist1(a)
	return x.(TrigNum).Sin()
}

func Dsqrt(a Any) Any {
	x := unlist1(a)
	return x.(TrigNum).Sqrt()
}

func Dstring(a Any) Any {
	return DlistZKZRstring(list1(a))
}

func DstringZKZRlist(a Any) Any {
	return DvectorZKZRlist(list1(DstringZKZRvector(a)))
}

func DstringZKZRnumber(a Any) Any {
	return list0()
}

func DstringZKZRsymbol(a Any) Any {
	return list0()
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
	return list0()
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

func DstringZKforZKeach(a Any) Any {
	return list0()
}

func DstringZKlength(a Any) Any {
	return list0()
}

func DstringZKmap(a Any) Any {
	return list0()
}

func DstringZKref(a Any) Any {
	o, k := unlist2(a)
	return o.(SString).Ref(k)
}

func DstringZKsetZA(a Any) Any {
	o, k, v := unlist3(a)
	return o.(SString).Set(k, v)
}

func DstringZPZQZS(a Any) Any {
	return list0()
}

func DstringZPZS(a Any) Any {
	return list0()
}

func DstringZQZS(a Any) Any {
	return list0()
}

func DstringZRZQZS(a Any) Any {
	return list0()
}

func DstringZRZS(a Any) Any {
	return list0()
}

func DstringZS(a Any) Any {
	return SBool(IsString(unlist1(a)))
}

func Dsubstring(a Any) Any {
	return list0()
}

func DsymbolZKZRstring(a Any) Any {
	return list0()
}

func DsymbolZS(a Any) Any {
	return SBool(IsSymbol(unlist1(a)))
}

func Dtan(a Any) Any {
	x := unlist1(a)
	return x.(TrigNum).Tan()
}

func DtextualZKportZS(a Any) Any {
	return SBool(IsTextualPort(unlist1(a)))
}

func Dtruncate(a Any) Any {
	return list0()
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
	return list0()
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
	fmt.Printf("%s\n", unlist1(a))
	return Void()
}

func DwriteZKbytevector(a Any) Any {
	bv, port := unlist1O(a, DcurrentZKoutputZKport(list0()))
	if !IsBinary(bv) {
		panic(newTypeError("write-bytevector expected bytevector"))
	}
	if !IsBinaryPort(port) {
		panic(newTypeError("write-bytevector expected binary-port"))
	}
	if !IsOutputPort(port) {
		panic(newTypeError("write-bytevector expected output-port"))
	}
	//err := port.(ByteWriter).WriteByte(byte(bv))
	//if err != nil {
	//	panic(err)
	//}
	return Void()
}

func DwriteZKchar(a Any) Any {
	ch, port := unlist1O(a, DcurrentZKoutputZKport(list0()))
	if !IsChar(ch) {
		panic(newTypeError("write-char expected char"))
	}
	if !IsTextualPort(port) {
		panic(newTypeError("write-char expected textual-port"))
	}
	if !IsOutputPort(port) {
		panic(newTypeError("write-char expected output-port"))
	}
	err := port.(RuneWriter).WriteRune(rune(ch.(SChar)))
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
	if !IsBinaryPort(port) {
		panic(newTypeError("write-u8 expected binary-port"))
	}
	if !IsOutputPort(port) {
		panic(newTypeError("write-u8 expected output-port"))
	}
	err := port.(ByteWriter).WriteByte(byte(ToFixnum(u8)))
	if err != nil {
		panic(err)
	}
	return list0()
}
