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
	"errors"
	"fmt"
	"os"
	"reflect"
	"runtime"
	"sort"
	"strings"
)

// structures and methods in this file
//
// SBool bool
// SChar rune
// SNull   struct
// SPair   struct
// SBinary struct
// SString struct
// SSymbol struct
// SVector struct
// SValues struct
// STable  struct
// *Env
// SPrimSyntax
// SCaseSyntax
// SRuleSyntax
// SPrim struct
// SProc struct
// SType struct
// SBytePort
// SCharPort

// each type defined in this file should have the following declarations
//
// func Is<type>(a Any) bool
// func New<type>(...) Any
// func (a S<type>) GetType() int
// func (a S<type>) Equal(b Any) bool

// ----------------------------------------------------------------------

// The Go language specification requires that methods are defined
// in the same package as the reciever type is defined, so if we
// don't do this, then gc will give us the following error:
//   "cannot define new methods on non-local type bool"
// Thus, in order to define methods we need our own type.

// boolean type

type SBool bool

// boolean methods

func IsBool(o Any) bool {
	var _, ok = o.(SBool)
	return ok
}

func (o SBool) GetType() int {
	return TypeCodeBool
}

func (o SBool) GetHash() uintptr {
	if o {
		return 1
	}
	return 0
}

func (o SBool) Equal(a Any) bool {
	return o == a.(SBool)
}

func (o SBool) String() string {
	if o {
		return "#t"
	}
	return "#f"
}

// character type

type SChar rune

// character methods

func IsChar(o Any) bool {
	var _, ok = o.(SChar)
	return ok
}

func (o SChar) GetType() int {
	return TypeCodeChar
}

func (o SChar) GetHash() uintptr {
	return uintptr(int(o))
}

func (o SChar) Equal(a Any) bool {
	if !IsChar(a) {
		return false
	}
	return o == a.(SChar)
}

func (o SChar) String() string {
	if 0x20 < o && o < 0x7F {
		return fmt.Sprintf("#\\%s", string([]byte{byte(o)}))
	}
	return fmt.Sprintf("#\\x%X", int(o))
}

// void type

type SVoid struct{}

func (o SVoid) GetType() int {
	return TypeCodeVoid
}

func (_ SVoid) Equal(a Any) bool {
	return IsType(a, TypeCodeVoid)
}

func (_ SVoid) String() string {
	return ""
}

// null type

type SNull struct{}

// null methods

func IsNull(o Any) bool {
	var _, ok = o.(SNull)
	return ok
}

func (o SNull) GetType() int {
	return TypeCodeNull
}

func (o SNull) GetHash() uintptr {
	return 0
}

func (_ SNull) Equal(a Any) bool {
	if !IsNull(a) {
		return false
	}
	return true
}

func (_ SNull) String() string {
	return "()"
}

func (o SNull) ToVector() Any {
	return SVector{it: []Any{}}
}

func (o SNull) Eval(env *Env) Any {
	return o
}

// s:pair type

type List struct {
	car Any
	cdr Any
}

// s:pair methods

func IsPair(o Any) bool {
	var _, ok = o.(*List)
	return ok
}

func (o *List) GetHash() uintptr {
	return 0 // TODO
}

func (o *List) GetType() int {
	return TypeCodePair
}

func (o *List) Equal(a Any) bool {
	return false // TODO
}

func (o *List) Eval(env *Env) Any {
	cas, cds := unlist1R(o)
	car := Deval(list2(cas, env))
	cdr := DevalZKliteral(list2(cds, env))
	return list1R(car, cdr)
}

func (o *List) String() string {
	if IsList(o) {
		v := listToVector(o)
		s := fmt.Sprintf("%s", v)
		return s[1:]
	}
	return fmt.Sprintf("(%s . %s)", o.car, o.cdr)
}

func (o *List) ToVector() Any {
	var ret = []Any{}
	var cur Any
	for cur = o; IsPair(cur); cur = cur.(*List).cdr {
		ret = append(ret, cur.(*List).car)
	}
	return SVector{it: ret}
}

func listToVector(a Any) SVector {
	switch a.(type) {
	case SNull:
		return a.(SNull).ToVector().(SVector)
	case *List:
		return a.(*List).ToVector().(SVector)
	}
	panic(newTypeError("list->vector expected list"))
}

func bindingsToPair(a Any) (ls, rs Any) {
	//fmt.Printf("bindingsToPair(%s)\n", a)
	var lhs = []Any{}
	var rhs = []Any{}
	var car *List
	var cur Any
	for cur = a; IsPair(cur); cur = cur.(*List).cdr {
		car = cur.(*List).car.(*List)
		lhs = append(lhs, car.car)
		rhs = append(rhs, car.cdr.(*List).car)		
	}
	ls = NewVector(lhs).ToList()
	rs = NewVector(rhs).ToList()
	return
}

func IsList(o Any) bool {
	// By definition, all lists are chains of pairs that have
	// finite length and are terminated by the empty list. [R6RS]

	// cycle detection (only needed in mutable model)
	switch o.GetType() {
	case TypeCodeNull:
		return true
	case TypeCodePair:
		return IsList(o.(*List).cdr)
	}
	return false
}

func Length(o Any) int {
	// cycle detection (only needed in mutable model)
	switch o.GetType() {
	case TypeCodePair:
		return 1 + Length(o.(*List).cdr)
	case TypeCodeNull:
		return 0
	}
	return 1
}

// s:bytevector type

type SBinary struct {
	bytes []byte
}

func IsBinary(o Any) bool {
	return IsType(o, TypeCodeBinary)
}

func (o SBinary) GetType() int {
	return TypeCodeBinary
}

func (o SBinary) GetHash() uintptr {
	return 0 // TODO
}

func (o SBinary) Equal(a Any) bool {
	return false // TODO
}

func (o SBinary) String() string {
	var ret string = ""
	for i := 0; i < len(o.bytes); i++ {
		ret += fmt.Sprintf(" %s", Sint64(o.bytes[i]))
	}
	return fmt.Sprintf("#u8(%s)", ret[1:])
}

// s:string type

type SString struct {
	text string
}

func IsString(a Any) bool {
	return IsType(a, TypeCodeString)
}

func NewString(s string) Any {
	return SString{text: s}
}

func (o SString) GetType() int {
	return TypeCodeString
}

func (o SString) GetHash() uintptr {
	return 0 // TODO
}

func (o SString) Equal(a Any) bool {
	return false // TODO
}

func (o SString) String() string {
	return fmt.Sprintf("\"%s\"", o.text)
}

// symbol type

type SSymbol struct {
	name string
}

func IsSymbol(o Any) bool {
	return IsType(o, TypeCodeSymbol)
}

func NewSymbol(s string) SSymbol {
	return SSymbol{name: s}
}

func (o SSymbol) GetType() int {
	return TypeCodeSymbol
}

func (o SSymbol) Equal(a Any) bool {
	return o.name == a.(SSymbol).name
}

func (o SSymbol) Eval(env *Env) Any {
	value := env.Ref(o)
	if value == nil {
		panic(newEvalError("variable not bound in environment: " + o.name))
	}
	return value
}

func (o SSymbol) String() string {
	return o.name
}

// vector type

type SVector struct {
	it []Any
}

func IsVector(a Any) bool {
	return IsType(a, TypeCodeVector)
}

func NewVector(a []Any) SVector {
	return SVector{it: a}
}

func (o SVector) ToList() Any {
	if len(o.it) == 0 {
		return SNull{}
	}
	return &List{o.it[0], NewVector(o.it[1:]).ToList()}
}

func (o SVector) GetType() int {
	return TypeCodeVector
}

func (o SVector) GetHash() uintptr {
	return 0 // TODO
}

func (o SVector) Equal(a Any) bool {
	return false // TODO
}

func (o SVector) Eval(env *Env) Any {
	var ret = DmakeZKvector(list1(Sint64(len(o.it)))).(SVector)
	for i := 0; i < len(o.it); i++ {
		ret.it[i] = Deval(list2(o.it[i], env))
	}
	return ret
}

func (o SVector) Ref(k Any) Any {
	return o.it[ToFixnum(k)]
}

func (o SVector) Set(k, v Any) Any {
	o.it[ToFixnum(k)] = v
	return Dvoid(list0())
}

func (o SVector) String() string {
	if len(o.it) == 0 {
		return "#()"
	}

	var ret string = ""
	for i := 0; i < len(o.it); i++ {
		ret += fmt.Sprintf(" %s", o.it[i])
	}
	return fmt.Sprintf("#(%s)", ret[1:])
}

func IsEmpty(a Any) bool {
	switch a.(type) {
	case SNull:
		return true
	case SBinary:
		return len(a.(SBinary).bytes) == 0
	case SString:
		return len(a.(SString).text) == 0
	case SSymbol:
		return len(a.(SSymbol).name) == 0
	case SVector:
		return len(a.(SVector).it) == 0
	case SValues:
		return len(a.(SValues).it) == 0
	}
	return false
}

// values type

type SValues struct {
	it []Any
}

// values methods

func (o SValues) GetType() int {
	return TypeCodeValues
}

func (o SValues) GetHash() uintptr {
	return 0 // TODO
}

func (o SValues) Equal(a Any) bool {
	return false
}

func (o SValues) String() string {
	if len(o.it) == 0 {
		return ""
	}
	return fmt.Sprintf("#<values:%s>", o.it)
}

// hashtable type

type STable struct {
	it map[Any]Any
}

func (o STable) GetType() int {
	return TypeCodeTable
}

func (o STable) GetHash() uintptr {
	return 0 // TODO
}

func (o STable) Equal(a Any) bool {
	return Equal(o, a)
}

func (o STable) String() string {
	return fmt.Sprintf("#<hashtable>")
}

// environment type

type Env struct {
	bound  map[string]Any
	parent *Env
}

func EmptyEnv() *Env {
	return &Env{bound: make(map[string]Any, 1024)}
}

func (env *Env) GetType() int {
	return TypeCodeEnvSpec
}

func (env *Env) Equal(a Any) bool {
	return false
}

func (env *Env) Extend() *Env {
	return &Env{bound: make(map[string]Any, 1024), parent: env}
}

func (env *Env) Has(symbol Any) bool {
	//fmt.Printf("Env.Has(%s)\n", symbol)
	if env.Ref(symbol) == nil {
		return false
	}
	return true
}

func (env *Env) Ref(symbol Any) Any {
	//fmt.Printf("Env.Ref(%s)\n", symbol)
	id := symbol.(SSymbol).String()
	if env.bound[id] != nil {
		return env.bound[id]
	}
	if env.parent == nil {
		return nil
	}
	return env.parent.Ref(symbol)
}

func (env *Env) Set(symbol, expr Any) Any {
	//fmt.Printf("Env.Set(%s) %s\n", symbol, expr)
	if !env.Has(symbol) {
		panic(newEvalError("set! variable must be prebound"))
	}
	value := Deval(list2(expr, env))

	// main logic
	id := symbol.(SSymbol).name
	env.bound[id] = value
	return values0()
}

func (env *Env) Define(symbol, body Any) Any {
	//fmt.Printf("Env.Define(%s) %s\n", symbol, body)
	var value Any
	if IsSymbol(symbol) {
		value = NewBegin(body, env)
	} else if IsPair(symbol) {
		var formals Any
		symbol, formals = unlist1R(symbol)
		value = NewLambda(list1R(formals, body), env)
	} else {
		panic(newEvalError("define: expected variable or list"))
	}
	if !IsSymbol(symbol) {
		panic(newEvalError("define: expected variable"))
	}

	// main logic
	id := symbol.(SSymbol).name
	env.bound[id] = value
	return values0()
}

func (env *Env) String() string {
	return fmt.Sprintf("#<environment with %d local bindings>", len(env.bound))
}

func (env *Env) dump() {
	if env.parent != nil {
		env.parent.dump()
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
}

func (env *Env) registerName(fn interface{}) string {
	// intuit function name
	pc := reflect.ValueOf(fn).Pointer()
	name := runtime.FuncForPC(pc).Name()

	// strip package name
	list := strings.Split(name, ".")
	name = list[len(list)-1]

	// strip first character
	return UnmangleName(name[1:])
}

func (env *Env) registerSyntax(fn func(Any, Any, *Env) Any) {
	n := env.registerName(fn)
	env.bound[n] = SPrimSyntax{form: fn, name: n}
}

func (env *Env) register(fn func(Any) Any) {
	n := env.registerName(fn)
	env.bound[n] = SPrim{call: fn, name: n}
}

func MangleName(name string) string {
	const table = "!\"#$%&'*+,-./:;<=>?@^`|~Z"
	var out = []byte{}
	var work = []byte(name)
	for i := 0; i < len(work); i++ {
		ch := work[i]
		ix := strings.Index(table, string(ch))
		if ix != -1 {
			out = append(out, 'Z', 'A'+byte(ix))
		} else {
			out = append(out, ch)
		}
	}
	return string(out)
}

func UnmangleName(mangled string) string {
	const table = "!\"#$%&'*+,-./:;<=>?@^`|~Z"
	var out = []byte{}
	var work = []byte(mangled)
	for i := 0; i < len(work); i++ {
		ch := work[i]
		if ch == 'Z' {
			i++
			ch := work[i]
			out = append(out, table[ch-'A'])
		} else {
			out = append(out, ch)
		}
	}
	return string(out)
}

// exception type

func PanicToError(expr Any) (value Any, err error) {
	x := recover()
	if x != nil {
		value = expr
		err = ToError(x)
		fmt.Printf("ERROR: %s\n", err)
	}
	return
}

func ErrorToPanic(value Any, err error) Any {
	if err != nil {
		panic(err)
	}
	return value
}

func ToError(a interface{}) error {
	switch a.(type) {
	case error:
		return a.(error)
	case string:
		return newEvalError(a.(string))
	}
	return newEvalError("unknown error")
}

func IsSyntax(kw Any, env *Env) bool {
	if env.Has(kw) && IsType(env.Ref(kw), TypeCodeSyntax) {
		return true
	}
	return false
}

func CountParens(s string) int {
	return strings.Count(s, "(") - strings.Count(s, ")")
}

// syntax type

type SPrimSyntax struct {
	form func(Any, Any, *Env) Any
	name string
}

type SCaseSyntax struct {
	env *Env
	expr Any
	lits Any
	body Any
	name string
}

type SRuleSyntax struct {
	env *Env
	lits Any
	body Any
	name string
}

// syntax methods

func (o SPrimSyntax) GetType() int {
	return TypeCodeSyntax
}

func (o SPrimSyntax) Equal(a Any) bool {
	return false
}

func (o SPrimSyntax) Transform(kw, st Any, env *Env) Any {
    return o.form(kw, st, env)
}

func (o SPrimSyntax) String() string {
	return fmt.Sprintf("#<syntax:%s>", o.name)
}

func (o SCaseSyntax) GetType() int {
	return TypeCodeSyntax
}

func (o SCaseSyntax) Equal(a Any) bool {
	return false
}

func (o SCaseSyntax) Transform(kw, st Any, env *Env) Any {
    return values0()
}

func (o SCaseSyntax) String() string {
	return fmt.Sprintf("#<syntax-case:%s>", o.name)
}

func (o SRuleSyntax) GetType() int {
	return TypeCodeSyntax
}

func (o SRuleSyntax) Equal(a Any) bool {
	return false
}

func (o SRuleSyntax) Transform(kw, st Any, env *Env) Any {
    return values0()
}

func (o SRuleSyntax) String() string {
	return fmt.Sprintf("#<syntax-rules:%s>", o.name)
}

// procedure types

type SPrim struct {
	call func(Any) Any
	name string
}

type SProc struct {
	env  *Env
	form Any
	body Any
	name string
}

// procedure methods

func NewPrim(fn func(Any) Any) Any {
	return SPrim{call: fn}
}

func NewBegin(body Any, env *Env) Any {
	return Kbegin(NewSymbol("begin"), body, env)
}

func NewLambda(rest Any, env *Env) Any {
	return Klambda(NewSymbol("lambda"), rest, env)
}

func IsProcedure(o Any) bool {
	return IsType(o, TypeCodeProc)
}

func (o SPrim) GetType() int {
	return TypeCodeProc
}

func (o SPrim) GetHash() uintptr {
	return 0 // TODO
}

func (o SPrim) Equal(a Any) bool {
	return false
}

func (o SPrim) Apply(a Any) Any {
	return o.call(a)
}

func (o SPrim) String() string {
	return fmt.Sprintf("#<procedure:%s>", o.name)
}

func (o SProc) GetType() int {
	return TypeCodeProc
}

func (o SProc) GetHash() uintptr {
	return SString{o.String()}.GetHash()
}

func (o SProc) Equal(a Any) bool {
	return false
}

func (o SProc) Apply(a Any) Any {
	body := o.body
	body = list1R(SSymbol{"begin"}, body)
	cenv := o.env.Extend()
	if IsSymbol(o.form) {
		cenv.bound[o.form.(SSymbol).name] = a
		return Deval(list2(body, cenv))
	}
	if !IsPair(o.form) {
		return Deval(list2(body, cenv))
	}

	// iterate over formal and actual arguments
	var bvar, bval Any
	for bvar, bval = o.form, a; IsPair(bvar) && IsPair(bval); 
	    bvar, bval = bvar.(*List).cdr, bval.(*List).cdr {
		cenv.bound[bvar.(*List).car.(SSymbol).name] = bval.(*List).car
	}

	// check for (a b c . rest) formal arguments
	if IsSymbol(bvar) {
		cenv.bound[bvar.(SSymbol).name] = bval
		return Deval(list2(body, cenv))
	}

	// check for argument mismatch
	switch {
	case IsNull(bvar) && !IsNull(bval):
		panic(newEvalError("lambda-apply expected less arguments"+body.(fmt.Stringer).String()))
	case !IsNull(bvar) && IsNull(bval):
		panic(newEvalError("lambda-apply expected more arguments"+body.(fmt.Stringer).String()))
	}

	return Deval(list2(body, cenv))
}

func (o SProc) String() string {
	return o.ToList().(fmt.Stringer).String()
}

func (o SProc) ToList() Any {
	return list2R(SSymbol{"lambda"}, o.form, o.body)
}

// type type

type SType struct {
	typeName       string
	typeCode       int
	portTypeCode   int
	numberTypeCode int
}

// type functions

func (o SType) GetType() int {
	return TypeCodeType
}

// this does not make SType a port because
// we do not implement the Read/Write methods
func (o SType) GetPortType() int {
	//return o.portType
	return 0 // TODO
}

func TypeCode(a Any) int {
	switch a.(type) {
	case SBool: return TypeCodeBool
	case SChar:	return TypeCodeChar
	case SNull: return TypeCodeNull  
	case *List: return TypeCodePair
	case SPrim: return TypeCodeProc
	case SProc: return TypeCodeProc
	//case SType: return TypeCodeType
	case SVoid:	return TypeCodeVoid
	case SBinary: return TypeCodeBinary
	case SString: return TypeCodeString
	case SSymbol: return TypeCodeSymbol
	case SValues: return TypeCodeValues
	case SVector: return TypeCodeVector
	case STable: return TypeCodeTable
	case *Env: return TypeCodeEnvSpec
	case SPrimSyntax: return TypeCodeSyntax
	case SCaseSyntax: return TypeCodeSyntax
	case SRuleSyntax: return TypeCodeSyntax
	}
	return TypeCodeAny
}

// port types

type ByteReader interface {
	ReadByte() (c byte, err error)
}

type ByteWriter interface {
	WriteByte(c byte) error
}

type RuneReader interface {
	ReadRune() (r rune, err error)
}

type RuneWriter interface {
	WriteRune(r rune) error
}

type SFilePort struct {
	it *os.File
}

type SStringPort SString

type SBinaryPort SBinary

func (o SFilePort) GetType() int {
	return TypeCodePort
}

func (o SFilePort) Equal(a Any) bool {
	return false // TODO
}

func (o SFilePort) ReadByte() (c byte, err error) {
	c = 0
	return
}

func (o SFilePort) WriteByte(c byte) error {
	return nil
}

func (o SFilePort) ReadRune() (r rune, err error) {
	r = 0
	return
}

func (o SFilePort) WriteRune(r rune) error {
	return nil
}

func newEvalError(s string) error {
	return errors.New("EvalError: " + s)
}

func newReadError(s string) error {
	return errors.New("ReadError: " + s)
}

func newSyntaxError(s string) error {
	return errors.New("SyntaxError: " + s)
}

func newTypeError(s string) error {
	return errors.New("TypeError: " + s)
}

func newWriteError(s string) error {
	return errors.New("WriteError: " + s)
}

// returns multiple values for argument handling
// so I don't think we need to export any of these

func list0() Any {
	return SNull{}
}

func list1(a Any) Any {
	return list1R(a, list0())
}

func list2(a, b Any) Any {
	return list1R(a, list1R(b, list0()))
}

func list3(a, b, c Any) Any {
	return list1R(a, list1R(b, list1R(c, list0())))
}

func list1R(a, rest Any) Any {
	return &List{a, rest}
}

func list2R(a, b, rest Any) Any {
	return list1R(a, list1R(b, rest))
}

func list3R(a, b, c, rest Any) Any {
	return list1R(a, list1R(b, list1R(c, rest)))
}

func listR(most, last Any) Any {
	//// this would have worked in the mutable model
	//var lastpair Any
	//for lastpair = most;
	//IsPair(lastpair.(*List).cdr);
	//lastpair = lastpair.(*List).cdr {}
	//lastpair.(*List).cdr = last
	//return most

	// immutable model requires reconstruction
	if IsPair(most) {
		return list1R(most.(*List).car, listR(most.(*List).cdr, last))
	}

	// assume IsNull
	return last
}

func unlist1(o Any) Any {
	return o.(*List).car
}

func unlist2(o Any) (a, b Any) {
	a = o.(*List).car
	b = o.(*List).cdr
	b = b.(*List).car
	return
}

func unlist3(o Any) (a, b, c Any) {
	a = o.(*List).car
	c = o.(*List).cdr
	b = c.(*List).car
	c = c.(*List).cdr
	c = c.(*List).car
	return
}

func unlist1R(o Any) (a Any, r Any) {
	a = o.(*List).car
	r = o.(*List).cdr
	return
}

func unlist2R(o Any) (a Any, b Any, r Any) {
	a = o.(*List).car
	r = o.(*List).cdr
	b = r.(*List).car
	r = r.(*List).cdr
	return
}

func unlist3R(o Any) (a Any, b Any, c Any, r Any) {
	a = o.(*List).car
	r = o.(*List).cdr
	b = r.(*List).car
	r = r.(*List).cdr
	c = r.(*List).car
	r = r.(*List).cdr
	return
}

func unlist1O(o Any, d Any) (a Any, r Any) {
	a = o.(*List).car
	r = o.(*List).cdr
    if IsPair(r) {
        r = r.(*List).car
    } else {
        r = d
    }
	return
}

func unlist2O(o Any, d Any) (a Any, b Any, r Any) {
	a = o.(*List).car
	r = o.(*List).cdr
	b = r.(*List).car
	r = r.(*List).cdr
    if IsPair(r) {
        r = r.(*List).car
    } else {
        r = d
    }
	return
}

// trying to make higher-order functions

func proc1(f func(Any) Any) func(Any) Any {
	return func(o Any) Any {
		var a = unlist1(o)
		return f(a)
	}
}

func proc2(f func(Any, Any) Any) func(Any) Any {
	return func(o Any) Any {
		var a, b = unlist2(o)
		return f(a, b)
	}
}

func proc3(f func(Any, Any, Any) Any) func(Any) Any {
	return func(o Any) Any {
		var a, b, c = unlist3(o)
		return f(a, b, c)
	}
}

func proc1R(f func(a, rest Any) Any) func(Any) Any {
	return func(o Any) Any {
		var a, rest = unlist1R(o)
		return f(a, rest)
	}
}

func proc2R(f func(a, b, rest Any) Any) func(Any) Any {
	return func(o Any) Any {
		var a, b, rest = unlist2R(o)
		return f(a, b, rest)
	}
}

func proc3R(f func(a, b, c, rest Any) Any) func(Any) Any {
	return func(o Any) Any {
		var a, b, c, rest = unlist3R(o)
		return f(a, b, c, rest)
	}
}

func unproc1(f func(Any) Any) func(Any) Any {
	return func(a Any) Any { return f(list1(a)) }
}
func unproc2(f func(Any) Any) func(Any, Any) Any {
	return func(a, b Any) Any { return f(list2(a, b)) }
}
func unproc3(f func(Any) Any) func(Any, Any, Any) Any {
	return func(a, b, c Any) Any { return f(list3(a, b, c)) }
}
func unproc1R(f func(Any) Any) func(Any, Any) Any {
	return func(a, rest Any) Any { return f(list1R(a, rest)) }
}
func unproc2R(f func(Any) Any) func(Any, Any, Any) Any {
	return func(a, b, rest Any) Any { return f(list2R(a, b, rest)) }
}
func unproc3R(f func(Any) Any) func(Any, Any, Any, Any) Any {
	return func(a, b, c, rest Any) Any { return f(list3R(a, b, c, rest)) }
}

// represents no return values
func values0() Any {
	return SValues{it: []Any{}}
}

// represents 2 return values
func values2(a, b Any) Any {
	return SValues{it: []Any{a, b}}
}

// represents multiple return values
func valuesR(rest Any) Any {
	vec := listToVector(rest)
	return SValues{it: vec.it}
}
