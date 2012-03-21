package droscheme

import (
	"fmt"
	"sort"
)

/* 
 * Procedures of the form K<mangled name> recieve arguments as
 *   s = (<keyword> <arg1> ... <argn>)
 * Procedures of the form D<mangled name> recieve arguments as
 *   a = (<arg1> ... <argn>)
 */

// (define var)
// (define var expr)
func Kdefine(s Any, env *Env) Any {
	ret, err := env.define(s.(SPair).cdr)
	if err != nil { panic(err) }
	return ret
}

func KdumpZKenvironment(s Any, env *Env) Any {
	if env.parent != nil {
		KdumpZKenvironment(s, env.parent)
		fmt.Printf("\t---\n")
	}
	keys := []string{}
	for k, _ := range env.bound {
		keys = append(keys, k)
	}
	sort.Sort(sort.StringSlice(keys))
	for _, key := range keys {
		fmt.Printf("\t%s=%s\n", key, env.bound[key])
	}
	return values0()
}

// (if c texpr)
// (if c texpr fexpr)
func Kif(s Any, env *Env) Any {
	_, test, texpr, rest := unlist3R(s)
	c, _ := Eval(test, env)
	if !IsBool(c) || bool(c.(SBool)) {
		x, err := Eval(texpr, env)
		if err != nil { panic(err) }
		return x
	}
	if IsPair(rest) {
		fexpr := unlist1(rest)
		x, err := Eval(fexpr, env)
		if err != nil { panic(err) }
		return x
	}
	return values0()
}

func Klambda(s Any, env *Env) Any {
	form, _ := unlist1R(s)
	// TODO
	return form
}

func Klibrary(s Any, env *Env) Any {
	return values0()
}

// (quote expr)
func Kquote(s Any, env *Env) Any {
	_, cds := unlist1R(s)
	return unlist1(cds)
}

// (set! var expr)
func KsetZA(s Any, env *Env) Any {
	ret, err := env.set(s.(SPair).cdr)
	if err != nil { panic(err) }
	return ret
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

/* (*) -- derived, but useful
 *
 * (define (* . rest)
 *   (fold-right num* 1 rest))
 */
func DZH(a Any) Any {
	return DfoldZKright(list3(SProc{call: DnumZH}, Sint64(1), a))
}

/* (+) -- derived, but useful
 *
 * (define (+ . rest)
 *   (fold-right num+ 0 rest))
 */
func DZI(a Any) Any {
	return DfoldZKright(list3(SProc{call: DnumZI}, Sint64(0), a))
}

/* (-) -- derived, but useful
 *
 * (define -
 *   (case-lambda
 *     (() 0)
 *     ((a) (num- 0 a))
 *     ((a . rest) (num- a (+ . rest)))))
 */
func DZK(a Any) Any {
	if IsNull(a) {
		return Sint64(0)
	}
	x, ys := unlist1R(a)
	if IsNull(ys) {
		return DnumZK(list2(Sint64(0), x))
	}
	return DnumZK(list2(x, DZI(ys)))
}

/* (/) -- derived, but useful
 *
 * (define /
 *   (case-lambda
 *     (() 1)
 *     ((a) (num/ 1 a))
 *     ((a . rest) (num/ a (* . rest)))))
 */
func DZM(a Any) Any {
	if IsNull(a) {
		return Sint64(1)
	}
	x, ys := unlist1R(a)
	if IsNull(ys) {
		return DnumZM(list2(Sint64(1), x))
	}
	return DnumZM(list2(x, DZH(ys)))
}

func Dabs(a Any) Any // derived, should be written in scheme

func Dappend(a Any) Any {
	return list0()
}

// (apply proc arg1 ... restargs)
func Dapply(a Any) Any {
	proc, restargs := unlist2(a)
	return Apply(proc, restargs)
}

func DbinaryZKportZS(a Any) Any {
	return SBool(IsBinaryPort(unlist1(a)))
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
	return Sint64(len(unlist1(a).(SBinary).bytes))
}

func DbytevectorZKu8ZKref(a Any) Any {
	o, k := unlist2(a)
	return Sint64(o.(SBinary).bytes[ToFixnum(k)])
}

func DbytevectorZKu8ZKsetZA(a Any) Any {
	o, k, v := unlist3(a)
	o.(SBinary).bytes[ToFixnum(k)] = byte(ToFixnum(v))
	return values0()
}

// R6RS:bytevector->u8-list
func DbytevectorZKZRu8ZKlist(a Any) Any {
	return DvectorZKZRlist(list1(DbytevectorZKZRu8ZKvector(a)))
}

func DbytevectorZKZRu8ZKvector(a Any) Any {
	bvec := unlist1(a).(SBinary)
	blen := len(bvec.bytes)
	vany := DmakeZKvector(list1(Sint64(blen)))
	v := vany.(SVector)
	for i := 0; i < blen; i++ {
		v.items[i] = Sint64(bvec.bytes[i])
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
	return list0()
}

func Dcar(a Any) Any {
	return unlist1(a).(SPair).car
}

func Dcdr(a Any) Any {
	return unlist1(a).(SPair).cdr
}

func Dceiling(a Any) Any {
	return list0()
}

func DcharZKZRinteger(a Any) Any {
	return Sint64(a.(SChar))
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
	return list0()
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
	var b, c = unlist2(a)
	return list1R(b, c)
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

func DdynamicZKwind(a Any) Any {
	return list0()
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

func Deval(a Any) Any {
	expr, _ := unlist1R(a)
	value, err := Eval(expr, BuiltinEnv())
	if err != nil {
		panic(err)
	}
	return value
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

func Dexpt(a Any) Any {
	return list0()
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
		return Apply(proc, list1(null))
	}
	if Length(rest) > 1 {
		panic("expected binary procedure")
	}
	ls := unlist1(rest)
	if IsNull(ls) {
		return null
	}
	ca, cd := unlist1R(ls)
	return Apply(proc, list2(ca, DfoldZKright(list3(proc, null, cd))))
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

func Dlength(a Any) Any {
	return Sint64(Length(unlist1(a)))
}

// this is the easiest function ever
func Dlist(a Any) Any {
	return a
}

func DlistZKZRstring(a Any) Any {
	return list0()
}

func DlistZKZRvector(a Any) Any {
	var vec = []Any{}
	for cur := unlist1(a); IsPair(cur); cur = cur.(SPair).cdr {
		vec = append(vec, cur.(SPair).car)
	}
	return SVector{vec, 0}
}

func DlistZKcopy(a Any) Any {
	// in mutable model we need to copy
	// in immutable model we don't
	return unlist1(a)
}

func DlistZKref(a Any) Any {
	list, ka := unlist2(a)
	k := ToFixnum(ka)
	for cur := list; IsPair(cur); k, cur = k-1, cur.(SPair).cdr {
		if k == 0 {
			return cur.(SPair).car
		}
	}
	panic(newTypeError("list-ref"))
}

func DlistZKtail(a Any) Any {
	list, ka := unlist2(a)
	k := ToFixnum(ka)
	for cur := list; IsPair(cur); k, cur = k-1, cur.(SPair).cdr {
		if k == 0 {
			return cur.(SPair).cdr
		}
	}
	panic(newTypeError("list-tail"))
}

func DlistZS(a Any) Any {
	return SBool(IsList(unlist1(a)))
}

func DmakeZM(a Any) Any {
	nany, dany := unlist2(a)
	n := ToFixnum(nany)
	d := ToFixnum(dany)
	return NewRational64(n, d)
}

func DmakeZKbytevector(a Any) Any {
	k, rest := unlist1R(a)
	var fill byte = 0
	if IsPair(rest) {
		fill = byte(ToFixnum(rest.(SPair).car))
	}
	n := int(ToFixnum(k))
	v := make([]byte, n, 256)
	for i := 0; i < n; i++ {
		v[i] = fill
	}
	return SBinary{bytes: v}
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

func DmakeZKstring(a Any) Any {
	return list0()
}

// (make-vector k fill?)
func DmakeZKvector(a Any) Any {
	k, rest := unlist1R(a)
	var fill Any = SNull{}
	if IsPair(rest) {
		fill = rest.(SPair).car
	}
	n := int(ToFixnum(k))
	v := make([]Any, n, 256)
	for i := 0; i < n; i++ {
		v[i] = fill
	}
	return SVector{items: v}
}

func Dmap(a Any) Any {
	return list0()
}

func Dmax(a Any) Any {
	return list0()
}

func Dmin(a Any) Any {
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

func DnullZS(a Any) Any {
	return SBool(IsNull(unlist1(a)))
}

func DnumZH(a Any) Any {
	ax, ay := unlist2(a); x := ax.(Num); y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return x.Mul(y)
}

func DnumZI(a Any) Any {
	ax, ay := unlist2(a); x := ax.(Num); y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return x.Add(y)
}

func DnumZK(a Any) Any {
	ax, ay := unlist2(a); x := ax.(Num); y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return x.Sub(y)
}

func DnumZM(a Any) Any {
	ax, ay := unlist2(a); x := ax.(Num); y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return x.Div(y)
}

func DnumZQ(a Any) Any {
	ax, ay := unlist2(a); x := ax.(Num); y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return SBool(x.(RealNum).Cmp(y) == 0)
}

func DnumZP(a Any) Any {
	ax, ay := unlist2(a); x := ax.(Num); y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return SBool(x.(RealNum).Cmp(y) == -1)
}

func DnumZR(a Any) Any {
	ax, ay := unlist2(a); x := ax.(Num); y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return SBool(x.(RealNum).Cmp(y) == 1)
}

func DnumZPZQ(a Any) Any {
	ax, ay := unlist2(a); x := ax.(Num); y := ay.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return SBool(x.(RealNum).Cmp(y) <= 0)
}

func DnumZRZQ(a Any) Any {
	ax, ay := unlist2(a); x := ax.(Num); y := ay.(Num)
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

func DrealZS(a Any) Any {
	return list0()
}

func Dreverse(a Any) Any {
	return list0()
}

func Dround(a Any) Any {
	return list0()
}

func Dstring(a Any) Any {
	return DlistZKZRstring(list1(a))
}

func DstringZKZRlist(a Any) Any {
	return list0()
}

func DstringZKZRnumber(a Any) Any {
	return list0()
}

func DstringZKZRsymbol(a Any) Any {
	return list0()
}

func DstringZKZRutf8(a Any) Any {
	return SBinary{bytes: []byte(unlist1(a).(SString).text)}
}

func DstringZKZRvector(a Any) Any {
	return list0()
}

func DstringZKappend(a Any) Any {
	return list0()
}

func DstringZKcopy(a Any) Any {
	return list0()
}

func DstringZKfillZA(a Any) Any {
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
	return list0()
}

func DstringZKsetZA(a Any) Any {
	return list0()
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

func DtextualZKportZS(a Any) Any {
	return SBool(IsTextualPort(unlist1(a)))
}

func Dtruncate(a Any) Any {
	return list0()
}

// R6RS:u8-list->bytevector
func Du8ZKlistZKZRbytevector(a Any) Any {
	var vec = []byte{}
	for cur := unlist1(a); IsPair(cur); cur = cur.(SPair).cdr {
		num := ToFixnum(cur.(SPair).car)
		if int64(byte(num)) != num {
			panic("TypeError: expected byte")
		}
		vec = append(vec, byte(num))
	}
	return SBinary{vec}
}

func Du8ZKvectorZKZRbytevector(a Any) Any {
	return Du8ZKlistZKZRbytevector(list1(DvectorZKZRlist(a)))
}

func Du8ZKreadyZS(a Any) Any {
	return list0()
}

func Dutf8ZKZRstring(a Any) Any {
	return SString{text: string(unlist1(a).(SBinary).bytes)}
}

func Dvalues(a Any) Any {
	return valuesR(a)
}

func Dvector(a Any) Any {
	return DlistZKZRvector(list1(a))
}

func DvectorZKZRlist(a Any) Any {
	vec := unlist1(a).(SVector)
	if len(vec.items) == 0 {
		return list0()
	}
	return list1R(vec.items[0], DvectorZKZRlist(list1(SVector{items: vec.items[1:]})))
}

func DvectorZKZRstring(a Any) Any {
	return list0()
}

func DvectorZKcopy(a Any) Any {
	return list0()
}

func DvectorZKfillZA(a Any) Any {
	return list0()
}

func DvectorZKforZKeach(a Any) Any {
	return list0()
}

func DvectorZKlength(a Any) Any {
	return Sint64(len(unlist1(a).(SVector).items))
}

func DvectorZKmap(a Any) Any {
	return list0()
}

func DvectorZKref(a Any) Any {
	o, k := unlist2(a)
	return o.(SVector).items[ToFixnum(k)]
}

func DvectorZKsetZA(a Any) Any {
	o, k, v := unlist3(a)
	o.(SVector).items[ToFixnum(k)] = v
	return values0()
}

func DvectorZS(a Any) Any {
	return SBool(IsVector(unlist1(a)))
}

func DwithZKexceptionZKhandler(a Any) Any {
	return list0()
}

func DwriteZKbytevector(a Any) Any {
	return list0()
}

func DwriteZKchar(a Any) Any {
	ch, port := unlist2(a)
	if !IsChar(ch) {
		panic(newTypeError("expected char"))
	}
	port.(Port).Write(ch)
	return values0()
}

func DwriteZKpartialZKbytevector(a Any) Any {
	return list0()
}

func DwriteZKu8(a Any) Any {
	return list0()
}

func DzeroZS(a Any) Any {
	return SBool(unlist1(a).(RealNum).Cmp(Sint64(0)) == 0)
}

//
// registration
//

// (ds builtin syntax)
func BuiltinSyntaxEnv() *Env {
	env := NullEnv()

	env.registerSyntax(Kdefine)
	env.registerSyntax(KdumpZKenvironment)
	env.registerSyntax(Kif)
	env.registerSyntax(Klambda)
	env.registerSyntax(Kquote)
	env.registerSyntax(KsetZA)

	return env
}

// (ds builtin)
func BuiltinEnv() *Env {
	env := ChildEnv(BuiltinSyntaxEnv())
	
	env.register(DZH)
	env.register(DZI)
	env.register(DZK)
	env.register(DZM)
	env.register(Dappend)
	env.register(Dapply)
	env.register(DbinaryZKportZS)
	env.register(DbooleanZS)
	env.register(DbytevectorZKcopy)
	env.register(DbytevectorZKcopyZA)
	env.register(DbytevectorZKcopyZKpartial)
	env.register(DbytevectorZKcopyZKpartialZA)
	env.register(DbytevectorZKlength)
	env.register(DbytevectorZKu8ZKref)
	env.register(DbytevectorZKu8ZKsetZA)
	env.register(DbytevectorZKZRu8ZKlist)
	env.register(DbytevectorZKZRu8ZKvector)
	env.register(DbytevectorZS)
	env.register(DcallZKwithZKcurrentZKcontinuation)
	env.register(DcallZKwithZKport)
	env.register(DcallZKwithZKvalues)
	env.register(DcallZMcc)
	env.register(Dcar)
	env.register(Dcdr)
	env.register(Dceiling)
	env.register(DcharZKZRinteger)
	env.register(DcharZKreadyZS)
	env.register(DcharZPZQZS)
	env.register(DcharZPZS)
	env.register(DcharZQZS)
	env.register(DcharZRZQZS)
	env.register(DcharZRZS)
	env.register(DcharZS)
	env.register(DcloseZKinputZKport)
	env.register(DcloseZKoutputZKport)
	env.register(DcloseZKport)
	env.register(DcomplexZS)
	env.register(Dcons)
	env.register(DcurrentZKerrorZKport)
	env.register(DcurrentZKinputZKport)
	env.register(DcurrentZKoutputZKport)
	env.register(Ddenominator)
	env.register(DdynamicZKwind)
	env.register(DeofZKobjectZS)
	env.register(DeqZS)
	env.register(DequalZS)
	env.register(DeqvZS)
	env.register(Derror)
	env.register(DerrorZKobjectZKirritants)
	env.register(DerrorZKobjectZKmessage)
	env.register(DerrorZKobjectZS)
	env.register(Deval)
	env.register(DexactZKZRinexact)
	env.register(DexactZKintegerZKsqrt)
	env.register(DexactZKintegerZS)
	env.register(DexactZS)
	env.register(Dexpt)
	env.register(Dfloor)
	env.register(DfoldZKleft)
	env.register(DfoldZKright)
	env.register(DflushZKoutputZKport)
	env.register(DforZKeach)
	env.register(DgetZKoutputZKbytevector)
	env.register(DgetZKoutputZKstring)
	env.register(Dhash)
	env.register(DinexactZKZRexact)
	env.register(DinexactZS)
	env.register(DinputZKportZS)
	env.register(DintegerZKZRchar)
	env.register(DintegerZS)
	env.register(Dlength)
	env.register(Dlist)
	env.register(DlistZKZRstring)
	env.register(DlistZKZRvector)
	env.register(DlistZKcopy)
	env.register(DlistZKref)
	env.register(DlistZKtail)
	env.register(DlistZS)
	env.register(DmakeZM)
	env.register(DmakeZKbytevector)
	env.register(DmakeZKlist)
	env.register(DmakeZKparameter)
	env.register(DmakeZKpolar)
	env.register(DmakeZKrectangular)
	env.register(DmakeZKstring)
	env.register(DmakeZKvector)
	env.register(Dmap)
	env.register(Dmax)
	env.register(Dmin)
	env.register(DnegativeZS)
	env.register(Dnewline)
	env.register(Dnot)
	env.register(DnullZS)
	env.register(DnumZH)
	env.register(DnumZI)
	env.register(DnumZK)
	env.register(DnumZM)
	env.register(DnumZQ)
	env.register(DnumZP)
	env.register(DnumZR)
	env.register(DnumZPZQ)
	env.register(DnumZRZQ)
	env.register(DnumberZKZRstring)
	env.register(DnumberZS)
	env.register(Dnumerator)
	env.register(DopenZKinputZKbytevector)
	env.register(DopenZKinputZKstring)
	env.register(DopenZKoutputZKbytevector)
	env.register(DopenZKoutputZKstring)
	env.register(DoutputZKportZS)
	env.register(DpairZS)
	env.register(DpeekZKchar)
	env.register(DpeekZKu8)
	env.register(DportZKopenZS)
	env.register(DportZS)
	env.register(DpositiveZS)
	env.register(DprocedureZS)
	env.register(Draise)
	env.register(DraiseZKcontinuable)
	env.register(DrationalZS)
	env.register(Drationalize)
	env.register(DreadZKbytevector)
	env.register(DreadZKbytevectorZA)
	env.register(DreadZKchar)
	env.register(DreadZKline)
	env.register(DreadZKu8)
	env.register(DrealZS)
	env.register(Dreverse)
	env.register(Dround)
	env.register(Dstring)
	env.register(DstringZKZRlist)
	env.register(DstringZKZRnumber)
	env.register(DstringZKZRsymbol)
	env.register(DstringZKZRutf8)
	env.register(DstringZKZRvector)
	env.register(DstringZKappend)
	env.register(DstringZKcopy)
	env.register(DstringZKfillZA)
	env.register(DstringZKforZKeach)
	env.register(DstringZKlength)
	env.register(DstringZKmap)
	env.register(DstringZKref)
	env.register(DstringZKsetZA)
	env.register(DstringZPZQZS)
	env.register(DstringZPZS)
	env.register(DstringZQZS)
	env.register(DstringZRZQZS)
	env.register(DstringZRZS)
	env.register(DstringZS)
	env.register(Dsubstring)
	env.register(DsymbolZKZRstring)
	env.register(DsymbolZS)
	env.register(DtextualZKportZS)
	env.register(Dtruncate)
	env.register(Du8ZKlistZKZRbytevector)
	env.register(Du8ZKvectorZKZRbytevector)
	env.register(Du8ZKreadyZS)
	env.register(Dutf8ZKZRstring)
	env.register(Dvalues)
	env.register(Dvector)
	env.register(DvectorZKZRlist)
	env.register(DvectorZKZRstring)
	env.register(DvectorZKcopy)
	env.register(DvectorZKfillZA)
	env.register(DvectorZKforZKeach)
	env.register(DvectorZKlength)
	env.register(DvectorZKmap)
	env.register(DvectorZKref)
	env.register(DvectorZKsetZA)
	env.register(DvectorZS)
	env.register(DwithZKexceptionZKhandler)
	env.register(DwriteZKbytevector)
	env.register(DwriteZKchar)
	env.register(DwriteZKpartialZKbytevector)
	env.register(DwriteZKu8)
	env.register(DzeroZS)

	return env
}