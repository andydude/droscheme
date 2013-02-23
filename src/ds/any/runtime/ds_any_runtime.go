package ds_any_runtime
import . "ds/any"
import (
	"fmt"
	"reflect"
)
var apply = NewProc(_apply, "apply")
func _apply(proc Any, args ...Any) Any {
	return proc.(Proc).Apply(Vector(args))
}
var booleanZS = NewProc(_booleanZS, "boolean?")
func _booleanZS(a Any) Any {
	_, ok := a.(bool)
	return ok
}
var booleanZQZS = NewProc(_booleanZQZS, "boolean=?")
func _booleanZQZS(a, b Any) Any {
	return (_booleanZS(a).(bool) && _booleanZS(b).(bool) && (a.(bool) == b.(bool)))
}
var bytevector = NewProc(_bytevector, "bytevector")
func _bytevector(rest ...Any) Any {
	return _u8ZKvectorZKZRbytevector(Vector(rest))
}
var bytevectorZKZRu8ZKlist = NewProc(_bytevectorZKZRu8ZKlist, "bytevector->u8-list")
func _bytevectorZKZRu8ZKlist(a Any) Any {
	return _vectorZKZRlist(_bytevectorZKZRu8ZKvector(a))
}
var bytevectorZKZRu8ZKvector = NewProc(_bytevectorZKZRu8ZKvector, "bytevector->u8-vector")
func _bytevectorZKZRu8ZKvector(a Any) Any {
	bv := a.(Binary)
	vc := _makeZKvector(len(bv)).(Vector)
	for i := 0; i < len(bv); i++ {
		vc[i] = bv[i]
	}
	return vc
}
var bytevectorZS = NewProc(_bytevectorZS, "bytevector?")
func _bytevectorZS(a Any) Any {
	_, ok := a.(Binary)
	return ok
}
var car = NewProc(_car, "car")
func _car(ls Any) Any {
	return ls.(*Pair).car
}
var carZIcdr = NewProc(_carZIcdr, "car+cdr")
func _carZIcdr(ls Any) (car, cdr Any) {
	return _car(ls), _cdr(ls)
}
var cdr = NewProc(_cdr, "cdr")
func _cdr(ls Any) Any {
	return ls.(*Pair).cdr
}
var charZKZRinteger = NewProc(_charZKZRinteger, "char->integer")
func _charZKZRinteger(ch Any) Any {
	return int(rune(ch.(Char)))
}
var charZQZS = NewProc(_charZQZS, "char=?")
func _charZQZS(a, b Any) Any {
	return (a.(Char) == b.(Char))
}
var charZS = NewProc(_charZS, "char?")
func _charZS(a Any) Any {
	_, ok := a.(Char)
	return ok
}
var cons = NewProc(_cons, "cons")
func _cons(a, b Any) Any {
	return &Pair{a, b}
}
var eofZKobject = NewProc(_eofZKobject, "eof-object")
func _eofZKobject() Any {
	return gEOF
}
var eofZKobjectZS = NewProc(_eofZKobjectZS, "eof-object?")
func _eofZKobjectZS(a Any) Any {
	if ch, ok := a.(Char); ok {
		return (ch == gEOF)
	}
	return false
}
var eqZS = NewProc(_eqZS, "eq?")
func _eqZS(a, b Any) Any {
	return _eqvZS(a, b)
}
var objectZKequalZS = NewProc(_objectZKequalZS, "object-equal?")
func _objectZKequalZS(a, b Any) Any {
	return reflect.DeepEqual(a, b)
}
var equalZS = NewProc(_equalZS, "equal?")
func _equalZS(a, b Any) Any {
	if c, ok := a.(Equaler); ok {
		return c.Equal(b)
	}
	if c, ok := b.(Equaler); ok {
		return c.Equal(a)
	}
	return _objectZKequalZS(a, b)
}
var eqvZS = NewProc(_eqvZS, "eqv?")
func _eqvZS(a, b Any) Any {
	if !_typeZQZS(a, b).(bool) {
		return false
	}
	if _symbolZS(a).(bool) {
		return _symbolZQZS(a, b)
	}
	if _booleanZS(a).(bool) {
		return _booleanZQZS(a, b)
	}
	if _inexactZS(a).(bool) {
		return _inexactZQZS(a, b)
	}
	if _exactZS(a).(bool) {
		return _exactZQZS(a, b)
	}
	if _charZS(a).(bool) {
		return _charZQZS(a, b)
	}
	if _nullZS(a).(bool) {
		return _nullZS(b)
	}
	if _emptyZS(a).(bool) {
		return _emptyZS(b)
	}
	return _pointerZQZS(a, b)
}
var __error = NewProc(_error, "error")
func _error(msg Any, irr ...Any) Any {
	return _raise(_errorZKobject(msg, irr...))
}
var errorZKobject = NewProc(_errorZKobject, "error-object")
func _errorZKobject(msg Any, irr ...Any) Any {
	str := _ZKZRimmutableZKstring(msg).(string)
	return ErrorObject{msg: str, it: Vector(irr)}
}
var errorZKobjectZKirritants = NewProc(_errorZKobjectZKirritants, "error-object-irritants")
func _errorZKobjectZKirritants(a Any) Any {
	return a.(Error).Irritants()
}
var errorZKobjectZKmessage = NewProc(_errorZKobjectZKmessage, "error-object-message")
func _errorZKobjectZKmessage(a Any) Any {
	return a.(Error).Error()
}
var errorZKobjectZS = NewProc(_errorZKobjectZS, "error-object?")
func _errorZKobjectZS(a Any) Any {
	_, ok := a.(Error)
	return ok
}
var exactZQZS = NewProc(_exactZQZS, "exact=?")
func _exactZQZS(a, b Any) Any {
	return _objectZKequalZS(a, b)
}
var exactZS = NewProc(_exactZS, "exact?")
func _exactZS(a Any) Any {
	if num, ok := a.(Num); ok {
		num.IsExact()
	}
	return false
}
var inexactZQZS = NewProc(_inexactZQZS, "inexact=?")
func _inexactZQZS(a, b Any) Any {
	return _objectZKequalZS(a, b)
}
var inexactZS = NewProc(_inexactZS, "inexact?")
func _inexactZS(a Any) Any {
	if num, ok := a.(Num); ok {
		num.IsInexact()
	}
	return false
}
var integerZKZRchar = NewProc(_integerZKZRchar, "integer->char")
func _integerZKZRchar(cp Any) Any {
	return Char(rune(cp.(int)))
}
var lastZKpair = NewProc(_lastZKpair, "last-pair")
func _lastZKpair(ls Any) Any {
	cur := ls
	if _nullZS(cur).(bool) {
		return _null()
	}
	for _pairZS(_cdr(cur)).(bool) {
		cur = _cdr(cur)
	}
	return cur
}
var length = NewProc(_length, "length")
func _length(ls Any) Any {
	if _nullZS(ls).(bool) {
		return 0
	}
	if _, ok := ls.(*Pair); ok {
		return 1 + _length(_cdr(ls)).(int)
	}
	return _error("length expected list")
}
var listZH = NewProc(_listZH, "list*")
func _listZH(o ...Any) Any {
	most, last := o[0:len(o)-1], []Any(o[len(o)-1].(Vector))
	vs := append(most, []Any(_listZKZRvector(last).(Vector)))
	return _vectorZKZRlist(Vector(vs))
}
var listZI = NewProc(_listZI, "list+")
func _listZI(a Any, rest ...Any) Any {
	first := []Any(_listZKZRvector(a).(Vector))
	return _vectorZKZRlist(Vector(append(first, rest...)))
}
var listZS = NewProc(_listZS, "list?")
func _listZS(a Any) Any {
	return _nullZS(_cdr(_lastZKpair(a)))
}
var list = NewProc(_list, "list")
func _list(o ...Any) Any {
	return _vectorZKZRlist(o)
}
var listZHZKZRvector = NewProc(_listZHZKZRvector, "list*->vector")
func _listZHZKZRvector(pr Any) Any {
	vec := []Any{}
	var cur Any
	for cur = pr; _pairZS(_cdr(cur)).(bool); cur = _cdr(cur) {
		vec = append(vec, _car(cur))
	}
	vec = append(vec, _car(cur), _cdr(cur))
	return Vector(vec)
}
var listZKZRstring = NewProc(_listZKZRstring, "list->string")
func _listZKZRstring(a Any) Any {
	return _vectorZKZRstring(_listZKZRvector(a))
}
var listZKZRvector = NewProc(_listZKZRvector, "list->vector")
func _listZKZRvector(ls Any) Any {
	if _nullZS(ls).(bool) {
		return Vector([]Any{})
	}
	if !_pairZS(ls).(bool) {
		_error("list->vector expected list")
	}
	cur, vc := _null(), []Any{}
	for cur = ls; _pairZS(cur).(bool); cur = _cdr(cur) {
		vc = append(vc, _car(cur))
	}
	if !_nullZS(cur).(bool) {
		_error("list->vector expected null")
	}
	return Vector(vc)
}
var makeZKbytevector = NewProc(_makeZKbytevector, "make-bytevector")
func _makeZKbytevector(k Any) Any {
	return Binary(make([]byte, k.(int)))
}
var makeZKlist = NewProc(_makeZKlist, "make-list")
func _makeZKlist(k Any) Any {
	return _vectorZKZRlist(_makeZKvector(k))
}
var makeZKstring = NewProc(_makeZKstring, "make-string")
func _makeZKstring(k Any) Any {
	return String(make([]rune, k.(int)))
}
var makeZKvector = NewProc(_makeZKvector, "make-vector")
func _makeZKvector(k Any) Any {
	return Vector(make([]Any, k.(int)))
}
var not = NewProc(_not, "not")
func _not(bl Any) Any {
	return !bl.(bool)
}
var null = NewProc(_null, "null")
func _null(o ...Any) Any {
	return gNull
}
var nullZS = NewProc(_nullZS, "null?")
func _nullZS(a Any) Any {
	_, ok := a.(*Null)
	return ok
}
var pairZS = NewProc(_pairZS, "pair?")
func _pairZS(a Any) Any {
	_, ok := a.(*Pair)
	return ok
}
var pointerZKof = NewProc(_pointerZKof, "pointer-of")
func _pointerZKof(a Any) Any {
	return reflect.ValueOf(a).Pointer()
}
var pointerZQZS = NewProc(_pointerZQZS, "pointer=?")
func _pointerZQZS(a, b Any) Any {
	return (_pointerZKof(a).(uintptr) == _pointerZKof(b).(uintptr))
}
var procedureZS = NewProc(_procedureZS, "procedure?")
func _procedureZS(a Any) Any {
	_, ok := a.(Proc)
	return ok
}
var raise = NewProc(_raise, "raise")
func _raise(err Any) Any {
	panic(err)
	return _void()
}
var __string = NewProc(_string, "string")
func _string(chars ...Any) Any {
	return _vectorZKZRstring(Vector(chars))
}
var stringZKhash = NewProc(_stringZKhash, "string-hash")
func _stringZKhash(a Any) Any {
	return NewString(a.(string)).GetHash()
}
var stringZKZRlist = NewProc(_stringZKZRlist, "string->list")
func _stringZKZRlist(a Any) Any {
	return _vectorZKZRlist(_stringZKZRvector(a))
}
var stringZKZRvector = NewProc(_stringZKZRvector, "string->vector")
func _stringZKZRvector(a Any) Any {
	st := a.(String)
	vc := _makeZKvector(len(st)).(Vector)
	for i := 0; i < len(st); i++ {
		vc[i] = st[i]
	}
	return vc
}
var stringZS = NewProc(_stringZS, "string?")
func _stringZS(a Any) Any {
	_, ok := a.(String)
	return ok
}
var symbolZQZS = NewProc(_symbolZQZS, "symbol=?")
func _symbolZQZS(a, b Any) Any {
	return (a.(Symbol).String() == b.(Symbol).String())
}
var stringZKZRsymbol = NewProc(_stringZKZRsymbol, "string->symbol")
func _stringZKZRsymbol(a Any) Any {
	return a.(String).ToSymbol()
}
var symbolZKZRstring = NewProc(_symbolZKZRstring, "symbol->string")
func _symbolZKZRstring(a Any) Any {
	return a.(Symbol).ToString()
}
var symbolZS = NewProc(_symbolZS, "symbol?")
func _symbolZS(a Any) Any {
	_, ok := a.(Symbol)
	return ok
}
var u8ZKlistZKZRbytevector = NewProc(_u8ZKlistZKZRbytevector, "u8-list->bytevector")
func _u8ZKlistZKZRbytevector(a Any) Any {
	return _u8ZKvectorZKZRbytevector(_listZKZRvector(a))
}
var u8ZKvectorZKZRbytevector = NewProc(_u8ZKvectorZKZRbytevector, "u8-vector->bytevector")
func _u8ZKvectorZKZRbytevector(a Any) Any {
	vc := a.(Vector)
	bv := _makeZKbytevector(len(vc)).(Binary)
	for i := 0; i < len(vc); i++ {
		bv[i] = vc[i].(byte)
	}
	return bv
}
var vectorZKZRlistZH = NewProc(_vectorZKZRlistZH, "vector->list*")
func _vectorZKZRlistZH(a Any) Any {
	vc := a.(Vector)
	if len(vc) == 0 {
		return _null()
	}
	if len(vc) == 1 {
		return vc[0]
	}
	return _cons(vc[0], _vectorZKZRlistZH(Vector(vc[1:])))
}
var vectorZKZRstring = NewProc(_vectorZKZRstring, "vector->string")
func _vectorZKZRstring(a Any) Any {
	vc := a.(Vector)
	st := _makeZKstring(len(vc)).(String)
	for i := 0; i < len(vc); i++ {
		st[i] = rune(vc[i].(Char))
	}
	return st
}
var vector = NewProc(_vector, "vector")
func _vector(o ...Any) Any {
	return Vector(o)
}
var vectorZKZRlist = NewProc(_vectorZKZRlist, "vector->list")
func _vectorZKZRlist(a Any) Any {
	vc, ls := a.(Vector), _null()
	for i := len(vc) - 1; i >= 0; i-- {
		ls = _cons(vc[i], ls)
	}
	return ls
}
var vectorZS = NewProc(_vectorZS, "vector?")
func _vectorZS(a Any) Any {
	_, ok := a.(Vector)
	return ok
}
var void = NewProc(_void, "void")
func _void(o ...Any) Any {
	return gVoid
}
var voidZS = NewProc(_voidZS, "void?")
func _voidZS(a Any) Any {
	_, ok := a.(*Void)
	return ok
}
var u8ZS = NewProc(_u8ZS, "u8?")
func _u8ZS(a Any) Any {
	if _, ok := a.(byte); ok {
		return true
	}
	if c, ok := a.(int); ok && ((0 <= c) && (c <= 255)) {
		return true
	}
	return false
}
var typeZQZS = NewProc(_typeZQZS, "type=?")
func _typeZQZS(a, b Any) Any {
	return (a.(AnyKinder).GetType() == b.(AnyKinder).GetType())
}
var stringZKZRimmutableZKstring = NewProc(_stringZKZRimmutableZKstring, "string->immutable-string")
func _stringZKZRimmutableZKstring(a Any) Any {
	if s, ok := a.(String); ok {
		return s.String()
	}
	return _error("expected string")
}
var immutableZKstringZKZRstring = NewProc(_immutableZKstringZKZRstring, "immutable-string->string")
func _immutableZKstringZKZRstring(a Any) Any {
	if s, ok := a.(string); ok {
		return NewString(s)
	}
	return _error("expected immutable-string")
}
var __ZKZRgoZKstring = NewProc(_ZKZRgoZKstring, "->go-string")
func _ZKZRgoZKstring(a Any) Any {
	if s, ok := a.(GoStringer); ok {
		return s.GoString()
	}
	return _error("->scheme-string unknown type")
}
var __ZKZRimmutableZKstring = NewProc(_ZKZRimmutableZKstring, "->immutable-string")
func _ZKZRimmutableZKstring(a Any) Any {
	if s, ok := a.(string); ok {
		return s
	}
	if s, ok := a.(fmt.Stringer); ok {
		return s.String()
	}
	return _error("->immutable-string unknown type")
}
var __ZKZRschemeZKstring = NewProc(_ZKZRschemeZKstring, "->scheme-string")
func _ZKZRschemeZKstring(a Any) Any {
	if s, ok := a.(SchemeStringer); ok {
		return s.SchemeString()
	}
	return _error("->scheme-string unknown type")
}
var __ZKZRstring = NewProc(_ZKZRstring, "->string")
func _ZKZRstring(a Any) Any {
	if s, ok := a.(String); ok {
		return s
	}
	return _immutableZKstringZKZRstring(_ZKZRimmutableZKstring(a))
}
