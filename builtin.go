package droscheme

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

func DtwoZH(a Any) Any {
	var x, y = unlist2(a)
	return Mul2(x.(Num), y.(Num))
}

func DtwoZI(a Any) Any {
	var x, y = unlist2(a)
	return Add2(x.(Num), y.(Num))
}

func DtwoZK(a Any) Any {
	var x, y = unlist2(a)
	return Sub2(x.(Num), y.(Num))
}

func DtwoZM(a Any) Any {
	var x, y = unlist2(a)
	return Div2(x.(Num), y.(Num))
}

func DZH(a Any) Any {
	// this is a hard one, for now, 2 arguments only
	return DtwoZH(a)
}

func DZI(a Any) Any {
	// this is a hard one, for now, 2 arguments only
	return DtwoZI(a)
}

func DZK(a Any) Any {
	// this is a hard one, for now, 2 arguments only
	return DtwoZK(a)
}

func DZM(a Any) Any {
	// this is a hard one, for now, 2 arguments only
	return DtwoZM(a)
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

func Dassoc(a Any) Any // derived
func Dassq(a Any) Any  // derived
func Dassv(a Any) Any  // derived

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
	return list0()
}
func DbytevectorZKu8ZKref(a Any) Any {
	return list0()
}
func DbytevectorZKu8ZKsetZA(a Any) Any {
	return list0()
}

// R6RS:bytevector->u8-list
func DbytevectorZKZRu8ZKlist(a Any) Any {
	return SNull{}
}

func DbytevectorZS(a Any) Any {
	return list0()
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
	// TODO: this is to easy
	return DequalZS(a)
}

func DequalZS(a Any) Any {
	var x, y = unlist2(a)
	return SBool(x.Equal(y))

}

func DeqvZS(a Any) Any {
	// TODO: this is to easy
	return DequalZS(a)
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
	env := CurrentEnv()
	out, _ := Eval(expr, env)
	return out
}

func DevenZS(a Any) Any {
	var n, ok = unlist1(a).(Num)
	if !ok {
		return SBool(false)
	}
	//if !IsInteger(n) { return SBool(false) }
	var mod = n.Mod1(n.FromI64(2)).Cmp1(n.FromI64(0))
	return SBool(mod == 0)
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
func DflushZKoutputZKport(a Any) Any {
	return list0()
}
func DforZKeach(a Any) Any {
	return list0()
}
func Dgcd(a Any) Any // derived
func DgetZKoutputZKbytevector(a Any) Any {
	return list0()
}
func DgetZKoutputZKstring(a Any) Any {
	return list0()
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
	return SChar(a.(Num).ToI64())
}

func DintegerZS(a Any) Any {
	return SBool(IsInteger(unlist1(a)))
}

func Dlcm(a Any) Any // derived

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
	k := ka.(Num).ToI64()
	for cur := list; IsPair(cur); k, cur = k-1, cur.(SPair).cdr {
		if k == 0 {
			return cur.(SPair).car
		}
	}
	panic("TypeError: what")
}

func DlistZKsetZA(a Any) Any // only in mutable model

func DlistZKtail(a Any) Any {
	list, ka := unlist2(a)
	k := ka.(Num).ToI64()
	for cur := list; IsPair(cur); k, cur = k-1, cur.(SPair).cdr {
		if k == 0 {
			return cur.(SPair).cdr
		}
	}
	panic("TypeError: what")
}

func DlistZS(a Any) Any {
	return SBool(IsList(unlist1(a)))
}

func DmakeZKbytevector(a Any) Any {
	return list0()
}
func DmakeZKlist(a Any) Any {
	return list0()
}
func DmakeZKparameter(a Any) Any {
	return list0()
}
func DmakeZKstring(a Any) Any {
	return list0()
}
func DmakeZKvector(a Any) Any {
	return list0()
}
func Dmap(a Any) Any {
	return list0()
}
func Dmax(a Any) Any {
	return list0()
}
func Dmember(a Any) Any {
	return list0()
}
func Dmemq(a Any) Any {
	return list0()
}
func Dmemv(a Any) Any {
	return list0()
}
func Dmin(a Any) Any {
	return list0()
}
func Dmodulo(a Any) Any {
	return list0()
}
func DnegativeZS(a Any) Any {
	return list0()
}
func Dnewline(a Any) Any {
	return list0()
}
func Dnot(a Any) Any {
	return list0()
}

func DnullZS(a Any) Any {
	return SBool(IsNull(unlist1(a)))
}

func DnumberZKZRstring(a Any) Any {
	return list0()
}
func DnumberZS(a Any) Any {
	return list0()
}
func Dnumerator(a Any) Any {
	return list0()
}

func DoddZS(a Any) Any {
	var n, ok = unlist1(a).(Num)
	if !ok {
		return SBool(false)
	}
	//if !IsInteger(n) { return SBool(false) }
	var mod = n.Mod1(n.FromI64(2)).Cmp1(n.FromI64(0))
	return SBool(mod != 0)
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
	return list0()
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
	return list0()
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
func Dremainder(a Any) Any{
	return list0()
}
func Dreverse(a Any) Any {
	return list0()
}
func Dround(a Any) Any {
	return list0()
}
func DsetZKcarZA(a Any) Any {
	return list0()
}
func DsetZKcdrZA(a Any) Any {
	return list0()
}
func Dstring(a Any) Any {
	return list0()
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
	return list0()
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
	return list0()
}
func Dsubstring(a Any) Any {
	return list0()
}
func DsymbolZKZRstring(a Any) Any {
	return list0()
}
func DsymbolZS(a Any) Any {
	return list0()
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
		num := cur.(SPair).car.(Num).ToI64()
		if int64(byte(num)) != num {
			panic("TypeError: expected byte")
		}
		vec = append(vec, byte(num))
	}
	return SBinary{vec}
}

func Du8ZKreadyZS(a Any) Any {
	return list0()
}

func Dutf8ZKZRstring(a Any) Any {
	return list0()
}
func Dvalues(a Any) Any {
	return valuesR(a)
}
func Dvector(a Any) Any {
	return list0()
}
func DvectorZKZRlist(a Any) Any {
	return list0()
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
	return list0()
}
func DvectorZKmap(a Any) Any {
	return list0()
}
func DvectorZKref(a Any) Any {
	return list0()
}
func DvectorZKset(a Any) Any {
	return list0()
}
func DvectorZS(a Any) Any {
	return list0()
}

func DwithZKexceptionZKhandler(a Any) Any {
	return list0()
}
func DwriteZKbytevector(a Any) Any {
	return list0()
}
func DwriteZKchar(a Any) Any {
	return list0()
}
func DwriteZKpartialZKbytevector(a Any) Any {
	return list0()
}
func DwriteZKu8(a Any) Any {
	return list0()
}
func DzeroZS(a Any) Any {
	return list0()
}

var globalCurrentEnv Env
func CurrentEnv() Env {
	return globalCurrentEnv
}

// (ds builtin)
func BuiltinEnv() Env {
	return Env{
	parent: nil,
	bound: map[string]Any{
	"*": SProc{DZH, "*"},
	"+": SProc{DZI, "+"},
	"-": SProc{DZK, "-"},
	"/": SProc{DZM, "/"},
	//"abs": SProc{Dabs, "abs"},
	"append": SProc{Dappend, "append"},
	"apply": SProc{Dapply, "apply"},
	//"assoc": SProc{Dassoc, "assoc"},
	//"assq": SProc{Dassq, "assq"},
	//"assv": SProc{Dassv, "assv"},
	"binary-port?": SProc{DbinaryZKportZS, "binary-port?"},
	"boolean?": SProc{DbooleanZS, "boolean?"},
	"bytevector-copy": SProc{DbytevectorZKcopy, "bytevector-copy"},
	"bytevector-copy!": SProc{DbytevectorZKcopyZA, "bytevector-copy!"},
	"bytevector-copy-partial": SProc{DbytevectorZKcopyZKpartial, "bytevector-copy-partial"},
	"bytevector-copy-partial!": SProc{DbytevectorZKcopyZKpartialZA, "bytevector-copy-partial!"},
	"bytevector-length": SProc{DbytevectorZKlength, "bytevector-length"},
	"bytevector-u8-ref": SProc{DbytevectorZKu8ZKref, "bytevector-u8-ref"},
	"bytevector-u8-set!": SProc{DbytevectorZKu8ZKsetZA, "bytevector-u8-set!"},
	"bytevector?": SProc{DbytevectorZS, "bytevector?"},
	"call-with-current-continuation": SProc{DcallZKwithZKcurrentZKcontinuation, "call-with-current-continuation"},
	"call-with-port": SProc{DcallZKwithZKport, "call-with-port"},
	"call-with-values": SProc{DcallZKwithZKvalues, "call-with-values"},
	"call/cc": SProc{DcallZMcc, "call/cc"},
	"car": SProc{Dcar, "car"},
	"cdr": SProc{Dcdr, "cdr"},
	"ceiling": SProc{Dceiling, "ceiling"},
	"char->integer": SProc{DcharZKZRinteger, "char->integer"},
	"char-ready?": SProc{DcharZKreadyZS, "char-ready?"},
	"char<=?": SProc{DcharZPZQZS, "char<=?"},
	"char<?": SProc{DcharZPZS, "char<?"},
	"char=?": SProc{DcharZQZS, "char=?"},
	"char>=?": SProc{DcharZRZQZS, "char>=?"},
	"char>?": SProc{DcharZRZS, "char>?"},
	"char?": SProc{DcharZS, "char?"},
	"close-input-port": SProc{DcloseZKinputZKport, "close-input-port"},
	"close-output-port": SProc{DcloseZKoutputZKport, "close-output-port"},
	"close-port": SProc{DcloseZKport, "close-port"},
	"complex?": SProc{DcomplexZS, "complex?"},
	"cons": SProc{Dcons, "cons"},
	"current-error-port": SProc{DcurrentZKerrorZKport, "current-error-port"},
	"current-input-port": SProc{DcurrentZKinputZKport, "current-input-port"},
	"current-output-port": SProc{DcurrentZKoutputZKport, "current-output-port"},
	"denominator": SProc{Ddenominator, "denominator"},
	"dynamic-wind": SProc{DdynamicZKwind, "dynamic-wind"},
	"eof-object?": SProc{DeofZKobjectZS, "eof-object?"},
	"eq?": SProc{DeqZS, "eq?"},
	"equal?": SProc{DequalZS, "equal?"},
	"eqv?": SProc{DeqvZS, "eqv?"},
	"error": SProc{Derror, "error"},
	"error-object-irritants": SProc{DerrorZKobjectZKirritants, "error-object-irritants"},
	"error-object-message": SProc{DerrorZKobjectZKmessage, "error-object-message"},
	"error-object?": SProc{DerrorZKobjectZS, "error-object?"},
	"eval": SProc{Deval, "eval"},
	"even?": SProc{DevenZS, "even?"},
	"exact->inexact": SProc{DexactZKZRinexact, "exact->inexact"},
	"exact-integer-sqrt": SProc{DexactZKintegerZKsqrt, "exact-integer-sqrt"},
	"exact-integer?": SProc{DexactZKintegerZS, "exact-integer?"},
	"exact?": SProc{DexactZS, "exact?"},
	"expt": SProc{Dexpt, "expt"},
	"floor": SProc{Dfloor, "floor"},
	"flush-output-port": SProc{DflushZKoutputZKport, "flush-output-port"},
	"for-each": SProc{DforZKeach, "for-each"},
	//"gcd": SProc{Dgcd, "gcd"},
	"get-output-bytevector": SProc{DgetZKoutputZKbytevector, "get-output-bytevector"},
	"get-output-string": SProc{DgetZKoutputZKstring, "get-output-string"},
	"inexact->exact": SProc{DinexactZKZRexact, "inexact->exact"},
	"inexact?": SProc{DinexactZS, "inexact?"},
	"input-port?": SProc{DinputZKportZS, "input-port?"},
	"integer->char": SProc{DintegerZKZRchar, "integer->char"},
	"integer?": SProc{DintegerZS, "integer?"},
	//"lcm": SProc{Dlcm, "lcm"},
	"length": SProc{Dlength, "length"},
	"list": SProc{Dlist, "list"},
	"list->string": SProc{DlistZKZRstring, "list->string"},
	"list->vector": SProc{DlistZKZRvector, "list->vector"},
	"list-copy": SProc{DlistZKcopy, "list-copy"},
	"list-ref": SProc{DlistZKref, "list-ref"},
	//"list-set!": SProc{DlistZKsetZA, "list-set!"},
	"list-tail": SProc{DlistZKtail, "list-tail"},
	"list?": SProc{DlistZS, "list?"},
	"make-bytevector": SProc{DmakeZKbytevector, "make-bytevector"},
	"make-list": SProc{DmakeZKlist, "make-list"},
	"make-parameter": SProc{DmakeZKparameter, "make-parameter"},
	"make-string": SProc{DmakeZKstring, "make-string"},
	"make-vector": SProc{DmakeZKvector, "make-vector"},
	"map": SProc{Dmap, "map"},
	"max": SProc{Dmax, "max"},
	//"member": SProc{Dmember, "member"},
	//"memq": SProc{Dmemq, "memq"},
	//"memv": SProc{Dmemv, "memv"},
	"min": SProc{Dmin, "min"},
	"negative?": SProc{DnegativeZS, "negative?"},
	"newline": SProc{Dnewline, "newline"},
	"not": SProc{Dnot, "not"},
	"null?": SProc{DnullZS, "null?"},
	"number->string": SProc{DnumberZKZRstring, "number->string"},
	"number?": SProc{DnumberZS, "number?"},
	"numerator": SProc{Dnumerator, "numerator"},
	"odd?": SProc{DoddZS, "odd?"},
	"open-input-bytevector": SProc{DopenZKinputZKbytevector, "open-input-bytevector"},
	"open-input-string": SProc{DopenZKinputZKstring, "open-input-string"},
	"open-output-bytevector": SProc{DopenZKoutputZKbytevector, "open-output-bytevector"},
	"open-output-string": SProc{DopenZKoutputZKstring, "open-output-string"},
	"output-port?": SProc{DoutputZKportZS, "output-port?"},
	"pair?": SProc{DpairZS, "pair?"},
	"peek-char": SProc{DpeekZKchar, "peek-char"},
	"peek-u8": SProc{DpeekZKu8, "peek-u8"},
	"port-open?": SProc{DportZKopenZS, "port-open?"},
	"port?": SProc{DportZS, "port?"},
	"positive?": SProc{DpositiveZS, "positive?"},
	"procedure?": SProc{DprocedureZS, "procedure?"},
	"raise": SProc{Draise, "raise"},
	"raise-continuable": SProc{DraiseZKcontinuable, "raise-continuable"},
	"rational?": SProc{DrationalZS, "rational?"},
	"rationalize": SProc{Drationalize, "rationalize"},
	"read-bytevector": SProc{DreadZKbytevector, "read-bytevector"},
	"read-bytevector!": SProc{DreadZKbytevectorZA, "read-bytevector!"},
	"read-char": SProc{DreadZKchar, "read-char"},
	"read-line": SProc{DreadZKline, "read-line"},
	"read-u8": SProc{DreadZKu8, "read-u8"},
	"real?": SProc{DrealZS, "real?"},
	"reverse": SProc{Dreverse, "reverse"},
	"round": SProc{Dround, "round"},
	//"set-car!": SProc{DsetZKcarZA, "set-car!"},
	//"set-cdr!": SProc{DsetZKcdrZA, "set-cdr!"},
	"string": SProc{Dstring, "string"},
	"string->list": SProc{DstringZKZRlist, "string->list"},
	"string->number": SProc{DstringZKZRnumber, "string->number"},
	"string->symbol": SProc{DstringZKZRsymbol, "string->symbol"},
	"string->utf8": SProc{DstringZKZRutf8, "string->utf8"},
	"string->vector": SProc{DstringZKZRvector, "string->vector"},
	"string-append": SProc{DstringZKappend, "string-append"},
	"string-copy": SProc{DstringZKcopy, "string-copy"},
	"string-fill!": SProc{DstringZKfillZA, "string-fill!"},
	"string-for-each": SProc{DstringZKforZKeach, "string-for-each"},
	"string-length": SProc{DstringZKlength, "string-length"},
	"string-map": SProc{DstringZKmap, "string-map"},
	"string-ref": SProc{DstringZKref, "string-ref"},
	"string<=?": SProc{DstringZPZQZS, "string<=?"},
	"string<?": SProc{DstringZPZS, "string<?"},
	"string=?": SProc{DstringZQZS, "string=?"},
	"string>=?": SProc{DstringZRZQZS, "string>=?"},
	"string>?": SProc{DstringZRZS, "string>?"},
	"string?": SProc{DstringZS, "string?"},
	"substring": SProc{Dsubstring, "substring"},
	"symbol->string": SProc{DsymbolZKZRstring, "symbol->string"},
	"symbol?": SProc{DsymbolZS, "symbol?"},
	"textual-port?": SProc{DtextualZKportZS, "textual-port?"},
	"u8-ready?": SProc{Du8ZKreadyZS, "u8-ready?"},
	"utf8->string": SProc{Dutf8ZKZRstring, "utf8->string"},
	"values": SProc{Dvalues, "values"},
	"vector": SProc{Dvector, "vector"},
	"vector->list": SProc{DvectorZKZRlist, "vector->list"},
	"vector->string": SProc{DvectorZKZRstring, "vector->string"},
	"vector-copy": SProc{DvectorZKcopy, "vector-copy"},
	"vector-fill!": SProc{DvectorZKfillZA, "vector-fill!"},
	"vector-for-each": SProc{DvectorZKforZKeach, "vector-for-each"},
	"vector-length": SProc{DvectorZKlength, "vector-length"},
	"vector-map": SProc{DvectorZKmap, "vector-map"},
	"vector-ref": SProc{DvectorZKref, "vector-ref"},
	"vector?": SProc{DvectorZS, "vector?"},
	"with-exception-handler": SProc{DwithZKexceptionZKhandler, "with-exception-handler"},
	"write-bytevector": SProc{DwriteZKbytevector, "write-bytevector"},
	"write-char": SProc{DwriteZKchar, "write-char"},
	"write-partial-bytevector": SProc{DwriteZKpartialZKbytevector, "write-partial-bytevector"},
	"write-u8": SProc{DwriteZKu8, "write-u8"},
	"zero?": SProc{DzeroZS, "zero?"},
	}}
}