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

func DZH(a Any) Any {
	// this is a hard one, for now, 2 arguments only
	var xa, ya = unlist2(a)
	var x, ok = xa.(Num)
	if !ok { panic("TypeError") }
	var y, oj = ya.(Num)
	if !oj { panic("TypeError") }
	return Mul2(x, y)
}

func DZI(a Any) Any {
	// this is a hard one, for now, 2 arguments only
	var xa, ya = unlist2(a)
	var x, ok = xa.(Num)
	if !ok { panic("TypeError") }
	var y, oj = ya.(Num)
	if !oj { panic("TypeError") }
	return Add2(x, y)
}

func DZK(a Any) Any {
	// this is a hard one, for now, 2 arguments only
	var xa, ya = unlist2(a)
	var x, ok = xa.(Num)
	if !ok { panic("TypeError") }
	var y, oj = ya.(Num)
	if !oj { panic("TypeError") }
	return Sub2(x, y)
}

func DZM(a Any) Any {
	// this is a hard one, for now, 2 arguments only
	var xa, ya = unlist2(a)
	var x, ok = xa.(Num)
	if !ok { panic("TypeError") }
	var y, oj = ya.(Num)
	if !oj { panic("TypeError") }
	return Div2(x, y)
}

func Dabs(a Any) Any // derived, should be written in scheme

func Dappend(a Any) Any

func Dapply(a Any) Any {
	// this must be part of s:eval
	return nil
}

func Dassoc(a Any) Any // derived
func Dassq(a Any) Any // derived
func Dassv(a Any) Any // derived

func DbinaryZKportZS(a Any) Any {
	return SBool(IsBinaryPort(unlist1(a)))
}

func DbooleanZS(a Any) Any {
	return SBool(IsBool(unlist1(a)))
}

func DbytevectorZKcopy(a Any) Any
func DbytevectorZKcopyZA(a Any) Any
func DbytevectorZKcopyZKpartial(a Any) Any
func DbytevectorZKcopyZKpartialZA(a Any) Any
func DbytevectorZKlength(a Any) Any
func DbytevectorZKu8ZKref(a Any) Any
func DbytevectorZKu8ZKsetZA(a Any) Any
func DbytevectorZS(a Any) Any

func DcallZKwithZKcurrentZKcontinuation(a Any) Any
func DcallZKwithZKport(a Any) Any
func DcallZKwithZKvalues(a Any) Any
func DcallZMcc(a Any) Any

func Dcar(a Any) Any {
	var p, ok = unlist1(a).(SPair)
	if !ok { panic("TypeError: expected Pair") }
	return p.car
}

func Dcdr(a Any) Any {
	var p, ok = unlist1(a).(SPair)
	if !ok { panic("TypeError: expected Pair") }
	return p.cdr
}

func Dceiling(a Any) Any

func DcharZKZRinteger(a Any) Any
func DcharZKreadyZS(a Any) Any
func DcharZPZQZS(a Any) Any
func DcharZPZS(a Any) Any
func DcharZQZS(a Any) Any
func DcharZRZQZS(a Any) Any
func DcharZRZS(a Any) Any

func DcharZS(a Any) Any {
	return SBool(IsChar(unlist1(a)))
}

func DcloseZKinputZKport(a Any) Any
func DcloseZKoutputZKport(a Any) Any
func DcloseZKport(a Any) Any
func DcomplexZS(a Any) Any

func Dcons(a Any) Any {
	var b, c = unlist2(a)
	return SPair{b, c}
}

func DcurrentZKerrorZKport(a Any) Any
func DcurrentZKinputZKport(a Any) Any
func DcurrentZKoutputZKport(a Any) Any
func Ddenominator(a Any) Any
func DdynamicZKwind(a Any) Any
func DeofZKobjectZS(a Any) Any

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

func Derror(a Any) Any
func DerrorZKobjectZKirritants(a Any) Any
func DerrorZKobjectZKmessage(a Any) Any
func DerrorZKobjectZS(a Any) Any

func DevenZS(a Any) Any {
	var n, ok = unlist1(a).(Num)
	if !ok { return SBool(false) }
	//if !IsInteger(n) { return SBool(false) }
	var mod = n.Mod1(n.FromI64(2)).Cmp1(n.FromI64(0))
	return SBool(mod == 0)
}

func DexactZKZRinexact(a Any) Any
func DexactZKintegerZKsqrt(a Any) Any
func DexactZKintegerZS(a Any) Any

func DexactZS(a Any) Any {
	var n, ok = a.(Num)
	if !ok { return SBool(false) }
	var t = n.GetNumberType()
	return SBool(t & NumberTypeCodeInexact == 0)
}

func Dexpt(a Any) Any // derived
func Dfloor(a Any) Any
func DflushZKoutputZKport(a Any) Any
func DforZKeach(a Any) Any
func Dgcd(a Any) Any // derived
func DgetZKoutputZKbytevector(a Any) Any
func DgetZKoutputZKstring(a Any) Any
func DinexactZKZRexact(a Any) Any

func DinexactZS(a Any) Any {
	var n, ok = a.(Num)
	if !ok { return SBool(false) }
	var t = n.GetNumberType()
	return SBool(t & NumberTypeCodeInexact != 0)
}

func DinputZKportZS(a Any) Any {
	return SBool(IsInputPort(unlist1(a)))
}

func DintegerZKZRchar(a Any) Any
func DintegerZS(a Any) Any
func Dlcm(a Any) Any // derived
func Dlength(a Any) Any

// this is the easiest function ever
func Dlist(a Any) Any {
	return a
}

func DlistZKZRstring(a Any) Any
func DlistZKZRvector(a Any) Any
func DlistZKcopy(a Any) Any
func DlistZKref(a Any) Any
func DlistZKsetZA(a Any) Any
func DlistZKtail(a Any) Any

func DlistZS(a Any) Any {
	return SBool(IsList(unlist1(a)))
}

func DmakeZKbytevector(a Any) Any
func DmakeZKlist(a Any) Any
func DmakeZKparameter(a Any) Any
func DmakeZKstring(a Any) Any
func DmakeZKvector(a Any) Any
func Dmap(a Any) Any
func Dmax(a Any) Any
func Dmember(a Any) Any
func Dmemq(a Any) Any
func Dmemv(a Any) Any
func Dmin(a Any) Any
func Dmodulo(a Any) Any
func DnegativeZS(a Any) Any
func Dnewline(a Any) Any
func Dnot(a Any) Any

func DnullZS(a Any) Any {
	return SBool(IsNull(unlist1(a)))
}

func DnumberZKZRstring(a Any) Any
func DnumberZS(a Any) Any
func Dnumerator(a Any) Any

func DoddZS(a Any) Any {
	var n, ok = unlist1(a).(Num)
	if !ok { return SBool(false) }
	//if !IsInteger(n) { return SBool(false) }
	var mod = n.Mod1(n.FromI64(2)).Cmp1(n.FromI64(0))
	return SBool(mod != 0)
}

func DopenZKinputZKbytevector(a Any) Any
func DopenZKinputZKstring(a Any) Any
func DopenZKoutputZKbytevector(a Any) Any
func DopenZKoutputZKstring(a Any) Any

func DoutputZKportZS(a Any) Any {
	return SBool(IsOutputPort(unlist1(a)))
}

func DpairZS(a Any) Any {
	return SBool(IsPair(unlist1(a)))
}

func DpeekZKchar(a Any) Any
func DpeekZKu8(a Any) Any
func DportZKopenZS(a Any) Any

func DportZS(a Any) Any {
	return SBool(IsPort(unlist1(a)))
}

func DpositiveZS(a Any) Any // derived
func DprocedureZS(a Any) Any
func Dquotient(a Any) Any // derived
func Draise(a Any) Any
func DraiseZKcontinuable(a Any) Any
func DrationalZS(a Any) Any
func Drationalize(a Any) Any
func DreadZKbytevector(a Any) Any
func DreadZKbytevectorZA(a Any) Any
func DreadZKchar(a Any) Any
func DreadZKline(a Any) Any
func DreadZKu8(a Any) Any
func DrealZS(a Any) Any
func Dremainder(a Any) Any // derived
func Dreverse(a Any) Any // derived
func Dround(a Any) Any
func DsetZKcarZA(a Any) Any
func DsetZKcdrZA(a Any) Any
func Dstring(a Any) Any
func DstringZKZRlist(a Any) Any
func DstringZKZRnumber(a Any) Any
func DstringZKZRsymbol(a Any) Any
func DstringZKZRutf8(a Any) Any
func DstringZKZRvector(a Any) Any
func DstringZKappend(a Any) Any
func DstringZKcopy(a Any) Any
func DstringZKfillZA(a Any) Any
func DstringZKforZKeach(a Any) Any
func DstringZKlength(a Any) Any
func DstringZKmap(a Any) Any
func DstringZKref(a Any) Any
func DstringZKsetZA(a Any) Any
func DstringZPZQZS(a Any) Any
func DstringZPZS(a Any) Any
func DstringZQZS(a Any) Any
func DstringZRZQZS(a Any) Any
func DstringZRZS(a Any) Any
func DstringZS(a Any) Any
func Dsubstring(a Any) Any
func DsymbolZKZRstring(a Any) Any
func DsymbolZS(a Any) Any

func DtextualZKportZS(a Any) Any {
	return SBool(IsTextualPort(unlist1(a)))
}

func Dtruncate(a Any) Any // derived
func Du8ZKreadyZS(a Any) Any
func Dutf8ZKZRstring(a Any) Any
func Dvalues(a Any) Any
func Dvector(a Any) Any
func DvectorZKZRlist(a Any) Any
func DvectorZKZRstring(a Any) Any
func DvectorZKcopy(a Any) Any
func DvectorZKfillZA(a Any) Any
func DvectorZKforZKeach(a Any) Any
func DvectorZKlength(a Any) Any
func DvectorZKmap(a Any) Any
func DvectorZKref(a Any) Any
func DvectorZKset(a Any) Any
func DvectorZS(a Any) Any

func DwithZKexceptionZKhandler(a Any) Any
func DwriteZKbytevector(a Any) Any
func DwriteZKchar(a Any) Any
func DwriteZKpartialZKbytevector(a Any) Any
func DwriteZKu8(a Any) Any
func DzeroZS(a Any) Any
