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
	"hash/crc32"
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
// SVoid   struct
// SNull   struct
// SPair   struct
// SBinary []byte
// SString []rune
// SSymbol struct
// SVector struct
// SValues struct
// SLabel  struct
// STable  struct
// SError  struct
// *Env
// SPrimSyntax
// SCaseSyntax
// SRuleSyntax
// SPrim struct
// SProc struct
// SCont struct
// SType struct
// *FilePort
// *BinaryPort
// *StringPort

// each type defined in this file should have the following declarations
//
// type S<type> struct {...}
//
// func Is<type>(a Any) bool
// func To<type>(...) S<type>
// func New<type>(...) Any
// func (a S<type>) GetType() int
// func (a S<type>) GetHash() uintptr
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

func (o SBool) Match(syntax Any, env *Env) bool {
	return o.Equal(syntax)
}

func (o SBool) Replace(env *Env) Any {
	return o
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

func (o SChar) Match(syntax Any, env *Env) bool {
	return o.Equal(syntax)
}

func (o SChar) Replace(env *Env) Any {
	return o
}

func (o SChar) Equal(a Any) bool {
	if !IsChar(a) {
		return false
	}
	return o == a.(SChar)
}

func (o SChar) String() string {
	if o == -1 {
		return "#<eof>"
	}
	if 0x20 < o && o < 0x7F {
		return fmt.Sprintf("#\\%s", string([]byte{byte(o)}))
	}
	return fmt.Sprintf("#\\x%X", int(o))
}

// void type

type SVoid struct{}

func IsVoid(a Any) bool {
	return IsType(a, TypeCodeVoid)
}

func Void() Any {
	return SVoid{}
}

func (o SVoid) GetHash() uintptr {
	return 0
}

func (o SVoid) GetType() int {
	return TypeCodeVoid
}

func (_ SVoid) Equal(a Any) bool {
	return IsType(a, TypeCodeVoid)
}

func (_ SVoid) String() string {
	// for debug
	return "#<unspecified>"
	//return ""
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

func (o SNull) Eval(env *Env) Any {
	return o
}

func (o SNull) Match(syntax Any, env *Env) bool {
	return o.Equal(syntax)
}

func (o SNull) Replace(env *Env) Any {
	return o
}

func (_ SNull) String() string {
	return "()"
}

func (o SNull) ToVector() Any {
	return SVector([]Any{})
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
	return seqHash([]Any(DlistZHZKZRvector(list1(o)).(SVector)))
}

func (o *List) GetType() int {
	return TypeCodePair
}

func (o *List) Equal(a Any) bool {
	return Equal(o, a)
}

func (o *List) Eval(env *Env) Any {
	cas, cds := unlist1R(o)
	car := Deval(list2(cas, env))
	cdr := DevalZKliteral(list2(cds, env))
	return list1R(car, cdr)
}

func (p *List) Match(syntax Any, env *Env) bool {
	pas, pds := unlist1R(p) // pattern

	//fmt.Printf("List.Match[ %s, %s ]\n", p, syntax)

	if IsPair(pds) {
		pads := pds.(*List).car
		if IsSymbol(pads) && pads.(SSymbol).String() == "..." {
			//fmt.Printf("= ellipsis = %s\n", syntax)
			if IsSymbol(pas) {
				if env.Has(pas) {
					KdumpZKenvironment(pas, pds, env)
					//fmt.Printf("= %s = %s\n", pas, env.Ref(pas))
					//fmt.Printf("= %s | %s\n", p, syntax)
					panic(newSyntaxError("list-match expected unbound symbol"))
				}
				if IsNull(syntax) || IsPair(syntax) {
					env.DefMatch(pas, syntax)
					//fmt.Printf("= #t 0\n")
					return true
				}
				//fmt.Printf("= #f 4\n")
				return false 
			}

			// TODO
			panic(newSyntaxError("ellipsis is only implemented for symbols"))
		}
	}

	cas, cds := unlist1R(syntax)

	if !pas.(Matcher).Match(cas, env) {
		//fmt.Printf("= #f 1\n")
		return false
	}
	if !pds.(Matcher).Match(cds, env) {
		//fmt.Printf("= #f 2\n")
		return false
	}
	//fmt.Printf("= #t 3\n")
	return true
}

func (t *List) Replace(env *Env) Any {
	tas, tds := unlist1R(t)

	if IsPair(tds) {
		tads := tds.(*List).car
		if IsSymbol(tads) && tads.(SSymbol).String() == "..." {
			return env.Ref(tas)
		}
	}

	car := tas.(Replacer).Replace(env)
	cdr := tds.(Replacer).Replace(env)
	return list1R(car, cdr)
}

func (o *List) String() string {
	return DshowZKlist(list1(o)).(SString).GoString()
}

func (o *List) ToVector() Any {
	var ret = []Any{}
	var cur Any
	for cur = o; IsPair(cur); cur = cur.(*List).cdr {
		ret = append(ret, cur.(*List).car)
	}
	return NewVector(ret)
}

func (o *List) First() Any {
	return o.car
}

func (o *List) Rest() Any {
	return o.cdr
}

func listToVector(a Any) SVector {
	switch {
	case IsNull(a):
		return a.(SNull).ToVector().(SVector)
	case IsPair(a):
		return a.(*List).ToVector().(SVector)
	}
	panic(newTypeError("list->vector expected list"))
}

func listToValues(a Any) Any {
	v := listToVector(a)
	if len(v) == 1 {
		return v[0]
	}
	return SValues(v)
}

func valuesToList(a Any) Any {
	if _, ok := a.(SValues); !ok {
		return list1(a)
	}
	return SVector(a.(SValues)).ToList()
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

// s:bytevector type

type SBinary []byte

func IsBinary(o Any) bool {
	return IsType(o, TypeCodeBinary)
}

func NewBinary(b []byte) SBinary {
	return SBinary(b)
}

func (o SBinary) GetType() int {
	return TypeCodeBinary
}

func (o SBinary) GetHash() uintptr {
	return seqHash([]Any(DbytevectorZKZRu8ZKvector(list1(o)).(SVector)))
}

func (o SBinary) Equal(a Any) bool {
	return Equal(o, a)
}

func (o SBinary) Match(syntax Any, env *Env) bool {
	return o.Equal(syntax)
}

func (o SBinary) Replace(env *Env) Any {
	return o
}

func (o SBinary) String() string {
	var ret string = ""
	for i := 0; i < len(o); i++ {
		ret += fmt.Sprintf(" %s", Sint64(o[i]))
	}
	return fmt.Sprintf("#u8(%s)", ret[1:])
}

func (o SBinary) Ref(k Any) Any {
	return Suint8(o[ToFixnum(k)])
}

func (o SBinary) Set(k, v Any) Any {
	o[ToFixnum(k)] = byte(ToFixnum(v))
	return Void()
}


// s:string type

type SString []rune

func IsString(a Any) bool {
	return IsType(a, TypeCodeString)
}

func ToString(s string) Any {
	return NewString([]rune(s))
}

func NewString(s []rune) SString {
	return SString(s)
}

func (o SString) GoString() string {
	return string([]rune(o))
}

func (o SString) GetType() int {
	return TypeCodeString
}

func (o SString) GetHash() uintptr {
	return seqHash([]Any(DstringZKZRvector(list1(o)).(SVector)))
}

func (o SString) Equal(a Any) bool {
	if !IsType(a, TypeCodeString) {
		return false
	}
	return Equal(o, a.(SString))
}

func (o SString) Match(syntax Any, env *Env) bool {
	return o.Equal(syntax)
}

func (o SString) Replace(env *Env) Any {
	return o
}

func (o SString) String() string {
	return fmt.Sprintf("\"%s\"", string([]rune(o)))
}

func (o SString) Ref(k Any) Any {
	return SChar(o[ToFixnum(k)])
}

func (o SString) Set(k, v Any) Any {
	o[ToFixnum(k)] = rune(ToFixnum(v))
	return Dvoid(list0())
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

func (o SSymbol) GetHash() uintptr {
	return DsymbolZKZRstring(list1(o)).GetHash()
}

func (o SSymbol) Equal(a Any) bool {
	if !IsSymbol(a) {
		return false
	}
	return o.name == a.(SSymbol).name
}

func (o SSymbol) Eval(env *Env) Any {
	value := env.Ref(o)
	if value == nil {
		panic(newEvalError("variable not bound in environment: " + o.name))
	}
	return value
}

func (o SSymbol) Match(syntax Any, env *Env) bool {
	// TODO: if o isin literals, then return true
	if o.name == "_" { return true }
	if o.name == "..." {
		panic("we were supposed to catch ... earlier")
		return false
	}
	if value := env.Ref(o); value != nil && o.Equal(value) {
		return o.Equal(syntax)
	}
	env.DefMatch(o, syntax)
	return true
}

func (o SSymbol) Replace(env *Env) Any {
	value := env.Ref(o)
	if value == nil {
		return o
	}
	return value
}

func (o SSymbol) String() string {
	return o.name
}

// vector type

type SVector []Any

func IsVector(a Any) bool {
	return IsType(a, TypeCodeVector)
}

func NewVector(a []Any) SVector {
	return SVector(a)
}

func (o SVector) ToList() Any {
	if len(o) == 0 {
		return SNull{}
	}
	return &List{o[0], NewVector(o[1:]).ToList()}
}

func (o SVector) GetType() int {
	return TypeCodeVector
}

func (o SVector) GetHash() uintptr {
	return seqHash([]Any(o))
}

func (o SVector) Equal(a Any) bool {
	return Equal(o, a)
}

func (o SVector) Eval(env *Env) Any {
	var ret = DmakeZKvector(list1(Sint64(len(o)))).(SVector)
	for i := 0; i < len(o); i++ {
		ret[i] = Deval(list2(o[i], env))
	}
	return ret
}

func (o SVector) Ref(k Any) Any {
	return o[ToFixnum(k)]
}

func (o SVector) Set(k, v Any) Any {
	o[ToFixnum(k)] = v
	return Dvoid(list0())
}

func (o SVector) String() string {
	return DshowZKvector(list1(o)).(SString).GoString()
}

func IsEmpty(a Any) bool {
	switch a.(type) {
	case SNull:
		return true
	case SVoid:
		return true
	case SBinary:
		return len(a.(SBinary)) == 0
	case SString:
		return len(a.(SString)) == 0
	case SSymbol:
		return len(a.(SSymbol).name) == 0
	case SVector:
		return len(a.(SVector)) == 0
	case SValues:
		return len(a.(SValues)) == 0
	}
	return false
}

// values type

type SValues []Any

// values methods

func IsValues(a Any) bool {
	return IsType(a, TypeCodeValues)
}

func NewValues(a []Any) Any {
	return SValues(a)
}

func (o SValues) GetType() int {
	return TypeCodeValues
}

func (o SValues) GetHash() uintptr {
	return 0 // TODO
}

func (o SValues) Equal(a Any) bool {
	return Equal(o, a)
}

func (o SValues) String() string {
	if len(o) == 0 {
		return "#<values>"
	}
	var ret string = ""
	for i := 0; i < len(o); i++ {
		ret += fmt.Sprintf("\n%s", o[i])
	}
	return fmt.Sprintf("#<values>%s", ret)
}

func (o SValues) First() Any {
	return o[0]
}

func (o SValues) Rest() SValues {
	return SValues(o[1:])
}

// label type

type SLabel struct {
	it Any
	label int
}

func NewLabel(l, d Any) Any {
	return SLabel{it: d, label: int(l.(SLabel).it.(Sint64))}
}

func (o SLabel) GetHash() uintptr {
	return 0
}

func (o SLabel) GetType() int {
	return TypeCodeLabel
}

func (o SLabel) Equal(a Any) bool {
	return false
}

func (o SLabel) String() string {
	if o.it == nil {
		return fmt.Sprintf("#%d#", o.label)
	}
	return fmt.Sprintf("#%d=%s", o.label, o.it)
}

// hashtable type

type STable struct {
	/*
	 * The Go specification states that map types can have
	 * any key types for which == and != are defined, which
	 * includes any type except for function, map, or slice.
	 * Droscheme uses ALOT of slice types, and so about 4 of the
	 * types that implement Any are slice types, so instead of
	 * map[Any]Any we have a map from uintptr (the hash type)
	 * to a bucket slice, which we iterate through for equality.
	 */
	it map[uintptr][]Any
	hashFn Any
	equivFn Any
}

func IsTable(a Any) bool {
	return IsType(a, TypeCodeTable)
}

func MakeTable(hash, equiv, k Any) STable {
	return STable{make(map[uintptr][]Any, ToFixnum(k)), hash, equiv}
}

func (o STable) GetType() int {
	return TypeCodeTable
}

func (o STable) GetHash() uintptr {
	return 0 // TODO
}

func (o STable) hash(a Any) uintptr {
	return uintptr(ToFixnum(o.hashFn.(Applier).Apply(list1(a))))
}

func (o STable) equiv(a, b Any) bool {
	return bool(o.equivFn.(Applier).Apply(list2(a, b)).(SBool))
}

func (o STable) dump() string {
	return fmt.Sprintf("%v", o.it)
}

func (o STable) Equal(a Any) bool {
	return Equal(o, a)
}

func (o STable) String() string {
	return fmt.Sprintf("#<hashtable:%s>", o.dump())
}

func (o STable) Ref(k Any) Any {
	const debug = false
	hash := o.hash(k)
	bucket := o.it[hash]
	if debug {
		fmt.Printf("hashtable-ref: found bucket %v\n", bucket)
	}
	if bucket == nil {
		return nil
	}
	for i := 0; i < len(bucket); i++ {
		pair := bucket[i].(*List)
		car, cdr := pair.car, pair.cdr
		if o.equiv(car, k) {
			if debug {
				fmt.Printf("hashtable-ref: found pair %v\n", pair)
			}
			return cdr
		}
	}
	return nil
}

func (o STable) Set(k, v Any) {
	const debug = false
	hash := o.hash(k)
	bucket := o.it[hash]
	if debug {
		fmt.Printf("hashtable-set!: found bucket %v\n", bucket)
	}
	if bucket == nil {
		o.it[hash] = []Any{list1R(k, v)}
		return
	}
	for i := 0; i < len(bucket); i++ {
		pair := bucket[i].(*List)
		if o.equiv(pair.car, k) {
			if debug {
				fmt.Printf("hashtable-set!: found pair %v\n", pair)
			}
			bucket[i] = list1R(k, v)
		}
	}
	bucket = append(bucket, list1R(k, v))
	return
}

func (o STable) Delete(k Any) {
	hash := o.hash(k)
	bucket := o.it[hash]
	fmt.Printf("hashtable-delete!: found bucket %v\n", bucket)
	if bucket == nil {
		return
	}
	buf := []Any{}
	for i := 0; i < len(bucket); i++ {
		pair := bucket[i].(*List)
		if o.equiv(pair.car, k) {
			fmt.Printf("hashtable-delete!: found pair %v\n", pair)
		} else {
			buf = append(buf, pair)
		}
	}
	bucket = buf
	return
	
}

func (o STable) Items() []Any {
	pairs := []Any{}
	for _, bucket := range o.it {
		for _, pair := range bucket {
			pairs = append(pairs, pair)
		}
	}
	//sort.Sort(sort.StringSlice(keys))
	return pairs
}

func (o STable) Entries() (keys []Any, values []Any) {
	keys = []Any{}
	values = []Any{}
	for _, bucket := range o.it {
		for _, pair := range bucket {
			keys = append(keys, pair.(*List).car)
			values = append(values, pair.(*List).cdr)
		}
	}
	return
}

func (o STable) Keys() []Any {
	keys, _ := o.Entries()
	return keys
}

func (o STable) Values() []Any {
	_, values := o.Entries()
	return values
}

type SError struct {
	err error
	it Any
	code int
}

func IsError(a Any) bool {
	return IsType(a, TypeCodeError)
}

func MakeError(err error) SError {
	return SError{err: err}
}

func NewError(msg, irr Any) SError {
	err := errors.New(msg.(SString).GoString())
	return SError{err: err, it: irr}
}

func (o SError) Equal(a Any) bool {
	return Equal(o, a)
}

func (o SError) GetType() int {
	return TypeCodeError
}

func (o SError) GetHash() uintptr {
	return SString(o.Error()).GetHash()
}

func (o SError) GetErrorType() int {
	return o.code
}

func (o SError) Irritants() Any {
	return o.it
}

func (o SError) Error() string {
	return o.err.Error()
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

func (env *Env) GetHash() uintptr {
	return 0 // TODO
}

func (env *Env) Equal(a Any) bool {
	return false
}

func (env *Env) Extend() *Env {
	return &Env{bound: make(map[string]Any, 1024), parent: env}
}

func (env *Env) Update(child *Env) *Env {
	// child environment must have no parent,
	// but if it does, then it will be ignored
	return &Env{bound: child.bound, parent: env}
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
	if env.parent != nil {
		return env.parent.Ref(symbol)
	}
	// this can't happen because .Has uses this code
	//panic(fmt.Sprintf("unbound variable '%s'", id))
	return nil
}

func (env *Env) Set(symbol, value Any) Any {
	fmt.Printf("Env.Set(%s) %s\n", symbol, value)
	//if !env.Has(symbol) {
	//	panic(newEvalError("set! variable must be prebound"))
	//}

	// main logic
	id := symbol.(SSymbol).name
	if env.bound[id] != nil {
		env.bound[id] = value
		return Void()
	}
	if env.parent != nil {
		return env.parent.Set(symbol, value)
	}

	panic(newEvalError("set! variable must be prebound"))
}

func (env *Env) DefValue(symbol, body Any) (id string, value Any) {
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
	id = symbol.(SSymbol).name
	return
}

func (env *Env) Define(symbol, body Any) Any {
	//fmt.Printf("define(%s, %s)\n", symbol, body)
	id, value := env.DefValue(symbol, body)
	env.bound[id] = value
	return Void()
}

func (env *Env) DefMatch(symbol, value Any) Any {
	id := symbol.(SSymbol).name
	env.bound[id] = value
	return Void()
}
func (env *Env) DefMacro(symbol, body Any) Any {
	id, value := env.DefValue(symbol, body)
	env.bound[id] = SProcSyntax{value.(SProc), id}
	return Void()
}

func (env *Env) DefSyntax(symbol, rules Any) Any {
	id := symbol.(SSymbol).name
	o := rules.(SRuleSyntax)
	env.bound[id] = SRuleSyntax{o.env, o.lits, o.body, id}
	return Void()
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

func getPC(fn interface{}) uintptr {
	return reflect.ValueOf(fn).Pointer()
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

	
func GetRootPath() string {
    return os.Getenv("DROSCHEME_ROOT")
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

type SProcSyntax struct {
	form SProc
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

func (o SPrimSyntax) Equal(a Any) bool {
	return false
}

func (o SPrimSyntax) GetHash() uintptr {
	return 0
}

func (o SPrimSyntax) GetType() int {
	return TypeCodeSyntax
}

func (o SPrimSyntax) Transform(kw, st Any, env *Env) Any {
    return o.form(kw, st, env)
}

func (o SPrimSyntax) String() string {
	return fmt.Sprintf("#<syntax:%s>", o.name)
}

func (o SProcSyntax) GetType() int {
	return TypeCodeSyntax
}

func (o SProcSyntax) GetHash() uintptr {
	return 0 // TODO
}

func (o SProcSyntax) Equal(a Any) bool {
	return false
}

func (o SProcSyntax) Transform(kw, st Any, env *Env) Any {
	//fmt.Printf("SProcSyntax.Transform\n")
	expr := o.form.Apply(st)
	//fmt.Println(expr)
    return Deval(list2(expr, env))
}

func (o SProcSyntax) String() string {
	return fmt.Sprintf("#<syntax:%s:>", o.name, o.form)
}

func (o SCaseSyntax) GetType() int {
	return TypeCodeSyntax
}

func (o SCaseSyntax) GetHash() uintptr {
	return 0 // TODO
}

func (o SCaseSyntax) Equal(a Any) bool {
	return false
}

func (o SCaseSyntax) Transform(kw, st Any, env *Env) Any {
	// TODO
    return Void()
}

func (o SCaseSyntax) String() string {
	return fmt.Sprintf("#<syntax-case:%s>", o.name)
}

func (o SRuleSyntax) GetType() int {
	return TypeCodeSyntax
}

func (o SRuleSyntax) GetHash() uintptr {
	return 0 // TODO
}

func (o SRuleSyntax) Equal(a Any) bool {
	return false
}

/* RuleSyntax.Transform
 *
 *
 */
func (o SRuleSyntax) Transform(kw, st Any, env *Env) Any {
	const debug = false
	/* (syntax-rules (literals ...)
	 *   (patterns templates) ...)
	 */
	// the input syntax 'st' is evaluated with env
	// the match(st, pat) produces a new env renv
	// the template syntax 'tmp' is evaluated with o.env.Update(renv)

	lenv := EmptyEnv() // literal environment
	for cur := o.lits; IsPair(cur); cur = cur.(*List).cdr {
		// this environment is also used as a set
		symbol := cur.(*List).car
		if !IsSymbol(symbol) {
			panic(newTypeError("syntax-rules expected symbol literal"))
		}
		lenv.DefMatch(symbol, symbol)
	}

	// for each in body
	for cur := o.body; IsPair(cur); cur = cur.(*List).cdr {
		// one syntax rule
		rule := cur.(*List).car

		// get pattern/template
		pat := Dcdr(list1(Dcar(list1(rule))))
		tmp := DlistZKref(list2(rule, Sint64(1)))

		if debug {
			fmt.Printf("RuleSyntax.Match() %s = %s\n", pat, st)
		}

		// get literals
		cenv := lenv.Extend()

		// phase 1: match
		if pat.(Matcher).Match(st, cenv) {
			// phase 2: replace
			expr := tmp.(Replacer).Replace(cenv)

			if debug {
				fmt.Printf("RuleSyntax.Transform() = %s\n", expr)
			}

			// phase 3: eval
			return Deval(list2(expr, env))
		}
	}

    return Void()
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

func NewEval(body Any, env *Env) Any {
	if _, ok := body.(SProc); !ok {
		panic("define-macro: expected lambda")
	}
	a := list3(NewSymbol("apply"), body, NewSymbol("R"))
	e := list3(NewSymbol("eval"), a, env)
	return NewLambda(list2(NewSymbol("R"), e), env)
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
	return fmt.Sprintf("#<primitive-procedure:%s>", o.name)
}

// lambda procedure type

func (o SProc) GetType() int {
	return TypeCodeProc
}

func (o SProc) GetHash() uintptr {
	//return NewString([]rune(o.String())).GetHash()
	return 0
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

// continuation type

type SCont struct {
	it Any
	code uintptr
}

func IsContinuation(a interface{}) bool {
	_, ok := a.(SCont)
	return ok
}

func (o SCont) GetType() int {
	return TypeCodeProc
}

func (o SCont) GetHash() uintptr {
	return 0
}

func (o SCont) Equal(a Any) bool {
	if !IsContinuation(a) { return false }
	return a.(SCont).code == o.code
}

func (o SCont) Apply(a Any) Any {
	o.it = a
	panic(o)
	return Void()
}

func (o SCont) String() string {
	return "#<continuation-procedure>"
}

// type type

type SType struct {
	typeName       string
	typeCode       int
	portTypeCode   int
	numberTypeCode int
}

// type functions

func (o SType) Equal(t Any) bool {
	return o.GetType() == t.GetType()
}

func (o SType) GetType() int {
	return TypeCodeType
}

// this does not make SType a port because
// we do not implement the Read/Write methods
func (o SType) GetPortType() int {
	//return o.portType
	return 0 // TODO
}

func typeOf(a Any) int {
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

func typeToString(tc int) string {
	switch tc {
	case TypeCodeBool:
		return "boolean"
	case TypeCodeChar:
		return "char"
	case TypeCodeNull:
		return "null"
	case TypeCodePair:
		return "pair"
	case TypeCodeProc:
		return "procedure"
	case TypeCodeVoid:
		return "void"
	case TypeCodeBinary:
		return "bytevector"
	case TypeCodeString:
		return "string"
	case TypeCodeSymbol:
		return "symbol"
	case TypeCodeValues:
		return "values"
	case TypeCodeVector:
		return "vector"
	case TypeCodeTable:
		return "hashtable"
	case TypeCodeNumber:
		return "number"
	case TypeCodePort:
		return "port"
	case TypeCodeEnvSpec:
		return "environment"
	case TypeCodeSyntax:
		return "syntax"
	}
	return "typeToString - WHAT!?!"
}

func seqHash(v []Any) uintptr {
	hash32 := crc32.NewIEEE()
	for i := 0; i < len(v); i++ {
		_, err := hash32.Write([]byte(fmt.Sprintf("%s ", v[i])))
		if err != nil {
			panic(err)
		}
	}
	return uintptr(hash32.Sum32())
}

func newError(s string) error {
	return errors.New(s)
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

// equivalent to '()
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

// equivalent to cons
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
	return DlistZI(list1R(most, last))
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

func unlist0O(o Any, d Any) Any {
    if IsPair(o) {
        return o.(*List).car
    }
	return d
}

func unlist1O(o Any, d Any) (a Any, r Any) {
	a = o.(*List).car
	r = o.(*List).cdr
	r = unlist0O(r, d)
	return
}

func unlist2O(o Any, d Any) (a Any, b Any, r Any) {
	a = o.(*List).car
	r = o.(*List).cdr
	b = r.(*List).car
	r = r.(*List).cdr
	r = unlist0O(r, d)
	return
}

func unlist3O(o Any, d Any) (a Any, b Any, c Any, r Any) {
	a = o.(*List).car
	r = o.(*List).cdr
	b = r.(*List).car
	r = r.(*List).cdr
	c = r.(*List).car
	r = r.(*List).cdr
	r = unlist0O(r, d)
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
	return NewValues([]Any{})
}

// represents 2 return values
func values2(a, b Any) Any {
	return NewValues([]Any{a, b})
}

