//
// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins, Daniel Connelly
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
//
package droscheme

import (
	"errors"
	"fmt"
	"hash/crc32"
	"os"
	"reflect"
	"runtime"
	"runtime/debug"
	"sort"
	"strings"
)

var gDebug bool = true

// structures and methods in this file
//
// Bool bool // deprecated
// Char rune // deprecated
// Void   struct
// SNull   struct
// SPair   struct
// Binary []byte
// String []rune
// Symbol struct
// Vector struct
// Values struct
// SLabel  struct
// SError  struct

// ds_bytevector

// ds_hashtable
// STable  struct
// *Env

// ds_syntax
// SPrimSyntax
// *CaseSyntax
// *RuleSyntax

// ds_procedure
// SPrim struct
// SProc struct
// SCont struct
// SType struct

// ds_io
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

type (
	Bool bool
	Char rune
	Void struct{}
	Null struct{}
	Pair struct {
		car Any
		cdr Any
	}
	Binary []byte
	String []rune
	Vector []Any
	Values []Any
	Symbol struct {
		name string
	}
	SLabel struct {
		it Any
		label int
	}
)
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


// ----------------------------------------------------------------------

// The Go language specification requires that methods are defined
// in the same package as the reciever type is defined, so if we
// don't do this, then gc will give us the following error:
//   "cannot define new methods on non-local type bool"
// Thus, in order to define methods we need our own type.

// boolean type


// boolean methods

func (o Bool) GetType() int {
	return TypeCodeBool
}

func (o Bool) GetHash() uintptr {
	if o {
		return 1
	}
	return 0
}

func (o Bool) Equal(a Any) bool {
	return o == a.(Bool)
}

func (o Bool) Match(syntax Any, env *Env) bool {
	return o.Equal(syntax)
}

func (o Bool) Replace(env *Env) Any {
	return o
}

func (o Bool) String() string {
	if o {
		return "#t"
	}
	return "#f"
}

// character type


// character methods

func (o Char) GetType() int {
	return TypeCodeChar
}

func (o Char) GetHash() uintptr {
	return uintptr(int(o))
}

func (o Char) Match(syntax Any, env *Env) bool {
	return o.Equal(syntax)
}

func (o Char) Replace(env *Env) Any {
	return o
}

func (o Char) Equal(a Any) bool {
	if !_charZS(a) {
		return false
	}
	return o == a.(Char)
}

func (o Char) String() string {
	if o == -1 {
		return "#<eof>"
	}
	if 0x20 < o && o < 0x7F {
		return fmt.Sprintf("#\\%s", string([]byte{byte(o)}))
	}
	return fmt.Sprintf("#\\x%X", int(o))
}


func listToVector(a Any) Vector {
	switch {
	case _nullZS(a):
		return a.(*Null).ToVector().(Vector)
	case _pairZS(a):
		return a.(*Pair).ToVector().(Vector)
	}
	panic(newTypeError("list->vector expected list"))
}

func listToValues(a Any) Any {
	v := listToVector(a)
	if len(v) == 1 {
		return v[0]
	}
	return Values(v)
}

func valuesToList(a Any) Any {
	if _, ok := a.(Values); !ok {
		return list1(a)
	}
	return Vector(a.(Values)).ToList()
}

func bindingsToPair(a Any) (ls, rs Any) {
	//fmt.Printf("bindingsToPair(%s)\n", a)
	var lhs = []Any{}
	var rhs = []Any{}
	var car *Pair
	var cur Any
	for cur = a; _pairZS(cur); cur = cur.(*Pair).cdr {
		car = cur.(*Pair).car.(*Pair)
		lhs = append(lhs, car.car)
		rhs = append(rhs, car.cdr.(*Pair).car)
	}
	ls = NewVector(lhs).ToList()
	rs = NewVector(rhs).ToList()
	return
}

// s:bytevector type

func ToBinary(s string) Any {
	return Binary([]byte(s))
}

// s:string type

func _stringZS(a Any) bool {
	return IsType(a, TypeCodeString)
}

func ToString(s string) Any {
	return NewString([]rune(s))
}

func NewString(s []rune) String {
	return String(s)
}

// symbol type

func _symbolZS(o Any) bool {
	return IsType(o, TypeCodeSymbol)
}

func NewSymbol(s string) Symbol {
	return Symbol{name: s}
}

func (o Symbol) GetType() int {
	return TypeCodeSymbol
}

func (o Symbol) GetHash() uintptr {
	return DsymbolZKZRstring(list1(o)).(Hasher).GetHash()
}

func (o Symbol) Equal(a Any) bool {
	if !_symbolZS(a) {
		return false
	}
	return o.name == a.(Symbol).name
}

func (o Symbol) Eval(env *Env) Any {
	value := env.Ref(o)
	if value == nil {
		panic(newEvalError("variable not bound in environment: " + o.name))
	}
	return value
}

func (o Symbol) Match(syntax Any, env *Env) bool {
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

func (o Symbol) Replace(env *Env) Any {
	value := env.Ref(o)
	if value == nil {
		return o
	}
	return value
}

func (o Symbol) String() string {
	return o.name
}

// vector type

func IsVector(a Any) bool {
	return IsType(a, TypeCodeVector)
}

func NewVector(a []Any) Vector {
	return Vector(a)
}

func (o Vector) ToList() Any {
	if len(o) == 0 {
		return list0()
	}
	return list1R(o[0], NewVector(o[1:]).ToList())
}

func (o Vector) GetType() int {
	return TypeCodeVector
}

func (o Vector) GetHash() uintptr {
	return seqHash([]Any(o))
}

func (o Vector) Equal(a Any) bool {
	return Equal(o, a)
}

func (o Vector) Eval(env *Env) Any {
	var ret = DmakeZKvector(list1(Sint64(len(o)))).(Vector)
	for i := 0; i < len(o); i++ {
		ret[i] = Eval(o[i], env)
	}
	return ret
}

func (o Vector) Ref(k Any) Any {
	return o[ToFixnum(k)]
}

func (o Vector) Set(k, v Any) Any {
	o[ToFixnum(k)] = v
	return Dvoid(list0())
}

func (o Vector) String() string {
	return DshowZKvector(list1(o)).(String).GoString()
}

func IsEmpty(a Any) bool {
	switch a.(type) {
	case *Null:
		return true
	case *Void:
		return true
	case Binary:
		return len(a.(Binary)) == 0
	case String:
		return len(a.(String)) == 0
	case Symbol:
		return len(a.(Symbol).name) == 0
	case Vector:
		return len(a.(Vector)) == 0
	case Values:
		return len(a.(Values)) == 0
	}
	return false
}

// values type

// values methods

func IsValues(a Any) bool {
	return IsType(a, TypeCodeValues)
}

func NewValues(a []Any) Any {
	return Values(a)
}

func (o Values) GetType() int {
	return TypeCodeValues
}

func (o Values) GetHash() uintptr {
	return 0 // TODO
}

func (o Values) Equal(a Any) bool {
	return Equal(o, a)
}

func (o Values) String() string {
	if len(o) == 0 {
		return "#<values>"
	}
	var ret string = ""
	for i := 0; i < len(o); i++ {
		ret += fmt.Sprintf("\n%s", o[i])
	}
	return fmt.Sprintf("#<values>%s", ret)
}

func (o Values) First() Any {
	return o[0]
}

func (o Values) Rest() Values {
	return Values(o[1:])
}

// label type

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
	return bool(o.equivFn.(Applier).Apply(list2(a, b)).(Bool))
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
	hash := o.hash(k)
	bucket := o.it[hash]
	if gDebug {
		fmt.Printf("hashtable-ref: found bucket %v\n", bucket)
	}
	if bucket == nil {
		return nil
	}
	for i := 0; i < len(bucket); i++ {
		pair := bucket[i].(*Pair)
		car, cdr := pair.car, pair.cdr
		if o.equiv(car, k) {
			if gDebug {
				fmt.Printf("hashtable-ref: found pair %v\n", pair)
			}
			return cdr
		}
	}
	return nil
}

func (o STable) Set(k, v Any) {
	hash := o.hash(k)
	bucket := o.it[hash]
	if gDebug {
		fmt.Printf("hashtable-set!: found bucket %v\n", bucket)
	}
	if bucket == nil {
		o.it[hash] = []Any{list1R(k, v)}
		return
	}
	for i := 0; i < len(bucket); i++ {
		pair := bucket[i].(*Pair)
		if o.equiv(pair.car, k) {
			if gDebug {
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
		pair := bucket[i].(*Pair)
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
			keys = append(keys, pair.(*Pair).car)
			values = append(values, pair.(*Pair).cdr)
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
	err := errors.New(msg.(String).GoString())
	return SError{err: err, it: irr}
}

func (o SError) Equal(a Any) bool {
	return Equal(o, a)
}

func (o SError) GetType() int {
	return TypeCodeError
}

func (o SError) GetHash() uintptr {
	return String(o.Error()).GetHash()
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

//type EnvSpec struct {
//	env *Env
//}

type Env struct {
	bound  map[string]Any
	parent *Env
}

func EmptyFrame() map[string]Any {
	return make(map[string]Any, 4)
}

func EmptyEnv() *Env {
	return &Env{bound: EmptyFrame()}
}

func toEnv(env Any) *Env {
	//return envs.env
	return env.(*Env)
}

func toEnvSpec(env *Env) *Env {
	//return EnvSpec{env}
	return env
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
	return &Env{bound: EmptyFrame(), parent: env}
}

func (env *Env) Update(child *Env) *Env {
	// child environment must have no parent,
	// but if it does, then it will be ignored
	return &Env{bound: child.bound, parent: env}
}

//func (env *Env) Has(symbol Any) bool {
//	//fmt.Printf("Env.Has(%s)\n", symbol)
//	if env.Ref(symbol) == nil {
//		return false
//	}
//	return true
//}

func (env *Env) Ref(symbol Any) Any {
	//fmt.Printf("Env.Ref(%s)\n", symbol)
	id := symbol.(Symbol).String()
	if env.bound[id] != nil {
		return env.bound[id]
	}
	if env.parent != nil {
		return env.parent.Ref(symbol)
	}
	return nil
}

func (env *Env) Set(symbol, value Any) Any {
	//fmt.Printf("Env.Set(%s) %s\n", symbol, value)
	id := symbol.(Symbol).name
	if env.bound[id] != nil {
		env.bound[id] = value
		return _void()
	}
	if env.parent != nil {
		return env.parent.Set(symbol, value)
	}

	panic("set! expected bound variable")
}

func (env *Env) DefValue(symbol, body Any) (id string, value Any) {
	if _symbolZS(symbol) {
		value = NewBegin(body, env)
	} else if _pairZS(symbol) {
		var formals Any
		symbol, formals = unlist1R(symbol)
		value = NewLambda(list1R(formals, body), env)
	} else {
		panic(newEvalError("define: expected variable or list"))
	}
	if !_symbolZS(symbol) {
		panic(newEvalError("define: expected variable"))
	}
	id = symbol.(Symbol).name
	return
}

func (env *Env) Define(symbol, body Any) Any {
	//fmt.Printf("define(%s, %s)\n", symbol, body)
	id, value := env.DefValue(symbol, body)
	env.bound[id] = value
	return _void()
}

func (env *Env) DefMatch(symbol, value Any) Any {
	id := symbol.(Symbol).name
	env.bound[id] = value
	return _void()
}
func (env *Env) DefMacro(symbol, body Any) Any {
	id, value := env.DefValue(symbol, body)
	env.bound[id] = &ProcSyntax{value.(*Proc), id}
	return _void()
}

func (env *Env) DefSyntax(symbol, rules Any) Any {
	id := symbol.(Symbol).name
	o := rules.(*RuleSyntax)
	env.bound[id] = &RuleSyntax{o.env, o.lits, o.body, id}
	return _void()
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
	env.bound[n] = &PrimSyntax{form: fn, name: n}
}

func (env *Env) register(fn func(Any) Any) {
	n := env.registerName(fn)
	env.bound[n] = &Prim{call: fn, name: n}
}

func (env *Env) registerGos(fn interface{}) {
	n := env.registerName(fn)
	env.bound[n] = &Prim{rcall: fn, name: n}
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

func error1panic(err error) {
	if err != nil {
		panic(err)
	}
	return
}

func error2panic(a Any, err error) Any {
	if err != nil {
		panic(err)
	}
	return a
}

func error3panic(a, b Any, err error) (c, d Any) {
	if err != nil {
		panic(err)
	}
	return a, b
}

func panic1error() (err error) {
	x := recover()
	if x != nil {
		debug.PrintStack()
		err = ToError(x)
		fmt.Printf("ERROR: %s\n", err)
	}
	return nil
}

func panic2error(a Any) (c Any, err error) {
	x := recover()
	if x != nil {
		debug.PrintStack()
		c = a
		err = ToError(x)
		fmt.Printf("ERROR: %s\n", err)
	}
	return a, nil
}

func panic3error(a, b Any) (c, d Any, err error) {
	x := recover()
	if x != nil {
		debug.PrintStack()
		c = a
		d = b
		err = ToError(x)
		fmt.Printf("ERROR: %s\n", err)
	}
	return a, b, nil
}

func ToError(a interface{}) error {
	switch a.(type) {
	case error:
		return a.(error)
	case string:
		return newEvalError(a.(string))
	}
	return newEvalError("unknown error type")
}

//func IsSyntax(kw Any, env *Env) bool {
//	if env.Has(kw) && IsType(env.Ref(kw), TypeCodeSyntax) {
//		return true
//	}
//	return false
//}

func CountParens(s string) int {
	return strings.Count(s, "(") - strings.Count(s, ")")
}

// syntax type

type PrimSyntax struct {
	form func(Any, Any, *Env) Any
	name string
}

type ProcSyntax struct {
	form *Proc
	name string
}

type CaseSyntax struct {
	env *Env
	expr Any
	lits Any
	body Any
	name string
}

type RuleSyntax struct {
	env *Env
	lits Any
	body Any
	name string
}

// syntax methods

func (o *PrimSyntax) Equal(a Any) bool {
	return false
}

func (o *PrimSyntax) GetHash() uintptr {
	return 0
}

func (o *PrimSyntax) GetType() int {
	return TypeCodeSyntax
}

func (o *PrimSyntax) Transform(kw, st Any, env *Env) Any {
    return o.form(kw, st, env)
}

func (o *PrimSyntax) String() string {
	return fmt.Sprintf("#<syntax:%s>", o.name)
}

func (o *ProcSyntax) GetType() int {
	return TypeCodeSyntax
}

func (o *ProcSyntax) GetHash() uintptr {
	return 0 // TODO
}

func (o *ProcSyntax) Equal(a Any) bool {
	return false
}

func (o *ProcSyntax) Transform(kw, st Any, env *Env) Any {
	expr := o.form.Apply(st)
    return Eval(expr, env)
}

func (o *ProcSyntax) String() string {
	return fmt.Sprintf("#<syntax:%s:%s>", o.name, o.form)
}

func (o *CaseSyntax) GetType() int {
	return TypeCodeSyntax
}

func (o *CaseSyntax) GetHash() uintptr {
	return 0 // TODO
}

func (o *CaseSyntax) Equal(a Any) bool {
	return false
}

func (o *CaseSyntax) Transform(kw, st Any, env *Env) Any {
	// TODO
    return _void()
}

func (o *CaseSyntax) String() string {
	return fmt.Sprintf("#<syntax-case:%s>", o.name)
}

func (o *RuleSyntax) GetType() int {
	return TypeCodeSyntax
}

func (o *RuleSyntax) GetHash() uintptr {
	return 0 // TODO
}

func (o *RuleSyntax) Equal(a Any) bool {
	return false
}

/* RuleSyntax.Transform
 *
 *
 */
func (o *RuleSyntax) Transform(kw, st Any, env *Env) Any {
	/* (syntax-rules (literals ...)
	 *   (patterns templates) ...)
	 */
	// the input syntax 'st' is evaluated with env
	// the match(st, pat) produces a new env renv
	// the template syntax 'tmp' is evaluated with o.env.Update(renv)

	lenv := EmptyEnv() // literal environment
	for cur := o.lits; _pairZS(cur); cur = cur.(*Pair).cdr {
		// this environment is also used as a set
		symbol := cur.(*Pair).car
		if !_symbolZS(symbol) {
			panic(newTypeError("syntax-rules expected symbol literal"))
		}
		lenv.DefMatch(symbol, symbol)
	}

	// for each in body
	for cur := o.body; _pairZS(cur); cur = cur.(*Pair).cdr {
		// one syntax rule
		rule := cur.(*Pair).car

		// get pattern/template
		pat := Dcdr(list1(Dcar(list1(rule)))) // cdar
		tmp := DlistZKref(list2(rule, Sint64(1))) // cadr

		if gDebug {
			fmt.Printf("RuleSyntax.Match() %s = %s\n", pat, st)
		}

		// get literals
		cenv := lenv.Extend()

		// phase 1: match
		if pat.(Matcher).Match(st, cenv) {
			// phase 2: replace
			expr := tmp.(Replacer).Replace(cenv)

			if gDebug {
				fmt.Printf("RuleSyntax.Transform() = %s\n", expr)
			}

			// phase 3: eval
			return Eval(expr, env)
		}
	}

    return _void()
}

func (o *RuleSyntax) String() string {
	return fmt.Sprintf("#<syntax-rules:%s>", o.name)
}

// procedure types

type Prim struct {
	rcall interface{}
	call func(Any) Any
	name string
}

type Proc struct {
	env  *Env
	form Any
	body Any
	name string
}

// procedure methods

func NewPrim(fn func(Any) Any) Any {
	return &Prim{call: fn}
}

func NewPrim2(fn func(Any) Any, name string) Any {
	return &Prim{call: fn, name: name}
}

//func NewEval(body Any, env *Env) Any {
//	if _, ok := body.(SProc); !ok {
//		panic("define-macro: expected lambda")
//	}
//	a := list3(NewSymbol("apply"), body, NewSymbol("R"))
//	e := list3(NewSymbol("eval"), a, env)
//	return NewLambda(list2(NewSymbol("R"), e), env)
//}

func NewBegin(body Any, env *Env) Any {
	return Kbegin(NewSymbol("begin"), body, env)
}

func NewLambda(rest Any, env *Env) Any {
	return Klambda(NewSymbol("lambda"), rest, env)
}

func IsProcedure(o Any) bool {
	return IsType(o, TypeCodeProc)
}

func (o *Prim) GetType() int {
	return TypeCodeProc
}

func (o *Prim) GetHash() uintptr {
	return 0 // TODO
}

func (o *Prim) Equal(a Any) bool {
	return false
}

func insToValues(a Any, arity int, rest bool) (vs []reflect.Value) {
	as := listToVector(a)
	vs = make([]reflect.Value, len(as))
	for i := 0; i < len(as); i++ {
		vs[i] = reflect.ValueOf(as[i])
	}
	//if rest {
	//	switch k {
	//	case 1:
	//		vs =
	//	case 2:
	//	case 3:
	//	case 4:
	//	case 5:
	//	default:
	//		panic("Prim.Apply expected less than 8 arguments")
	//	}
	//} else {
	//	switch k {
	//	case 0:
	//		vs = {a.(L)}
	//	case 1:
	//	case 2:
	//	case 3:
	//	case 4:
	//	case 5:
	//	default:
	//		panic("Prim.Apply expected less than 8 arguments")
	//	}
	//}
	return
}

func valuesToOuts(vs []reflect.Value) Any {
	if len(vs) == 1 {
		return vs[0].Interface().(Any)
	}
	as := make([]Any, len(vs))
	for i := 0; i < len(vs); i++ {
		as[i] = vs[i].Interface().(Any)
	}
	return NewValues(as)
}

func (o *Prim) reflectApply(a Any) Any {
	ov := reflect.ValueOf(o.rcall)
	ot := reflect.TypeOf(o.rcall)
	return valuesToOuts(ov.Call(insToValues(a, ot.NumIn(), ot.IsVariadic())))
}

func (o *Prim) Apply(a Any) Any {
	if o.call != nil {
		return o.call(a)
	}

	if o.rcall != nil {
		return o.reflectApply(a)
	}
	//r := []rune(GetFN(GetPC(o.call)))[0]
	//switch r {
	//case '_':
	//case 'D':
	//}
	panic("Prim.Apply expected _ prefix")
}

func (o *Prim) String() string {
	return fmt.Sprintf("#<primitive-procedure:%s>", o.name)
}

// lambda procedure type

func (o *Proc) GetType() int {
	return TypeCodeProc
}

func (o *Proc) GetHash() uintptr {
	//return NewString([]rune(o.String())).GetHash()
	return 0
}

func (o *Proc) Equal(a Any) bool {
	return false
}

func (o *Proc) Apply(a Any) Any {
	body := o.body
	body = list1R(Symbol{"begin"}, body)
	cenv := o.env.Extend()

	// (lambda rest body)
	if _symbolZS(o.form) {
		cenv.bound[o.form.(Symbol).name] = a
		return Eval(body, cenv)
	}

	// (lambda () body)
	if !_pairZS(o.form) {
		return Eval(body, cenv)
	}

	// iterate over formal and actual arguments
	var bvar, bval Any
	for bvar, bval = o.form, a; _pairZS(bvar) && _pairZS(bval);
	    bvar, bval = bvar.(*Pair).cdr, bval.(*Pair).cdr {
		cenv.bound[bvar.(*Pair).car.(Symbol).name] = bval.(*Pair).car
	}

	// (lambda (a b c . rest) body)
	if _symbolZS(bvar) {
		cenv.bound[bvar.(Symbol).name] = bval
		return Eval(body, cenv)
	}

	// check for argument mismatch
	switch {
	case _nullZS(bvar) && !_nullZS(bval):
		panic(newEvalError("lambda-apply expected less arguments"+body.(fmt.Stringer).String()))
	case !_nullZS(bvar) && _nullZS(bval):
		panic(newEvalError("lambda-apply expected more arguments"+body.(fmt.Stringer).String()))
	}

	return Eval(body, cenv)
}

func (o *Proc) String() string {
	return o.ToList().(fmt.Stringer).String()
}

func (o *Proc) ToList() Any {
	return list2R(Symbol{"lambda"}, o.form, o.body)
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
	return _void()
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
	return o.GetType() == t.(AnyKinder).GetType()
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

func SetCmdLine(args []string) {
	aargs := []Any{}

	for _, str := range args {
		aargs = append(aargs, ToString(str))
		if str == "-f" {
			aargs = []Any{}
		}
	}
	gCmdLine.(Applier).Apply(list1(NewVector(aargs).ToList()))
}

func NewKeyword(key, value Any) Any {
	return list3(NewSymbol("#%key"), list2(NewSymbol("quote"), key), value)
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

func Car(l Any) Any {
	return l.(*Pair).car
}

func Cdr(l Any) Any {
	return l.(*Pair).cdr
}

// equivalent to '()
func list0() Any {
	return gNull
}

func list1(a Any) Any {
	return list1R(a, list0())
}

func list2(a, b Any) Any {
	return list2R(a, b, list0())
}

func list3(a, b, c Any) Any {
	return list3R(a, b, c, list0())
}

// equivalent to cons
func list1R(a, r Any) Any {
	return &Pair{a, r}
}

func list2R(a, b, r Any) Any {
	return &Pair{a, &Pair{b, r}}
}

func list3R(a, b, c, r Any) Any {
	return &Pair{a, &Pair{b, &Pair{c, r}}}
}

func listR(most, last Any) Any {
	return DlistZI(list1R(most, last))
}

func unlist1(o Any) Any {
	return Car(o)
}

func unlist2(o Any) (a, b Any) {
	a = Car(o)
	b = Cdr(o)
	b = Car(b)
	return
}

func unlist3(o Any) (a, b, c Any) {
	a = Car(o)
	c = Cdr(o)
	b = Car(c)
	c = Cdr(c)
	c = Car(c)
	return
}

func unlist1R(o Any) (a Any, r Any) {
	a = Car(o)
	r = Cdr(o)
	return
}

func unlist2R(o Any) (a Any, b Any, r Any) {
	a = Car(o)
	r = Cdr(o)
	b = Car(r)
	r = Cdr(r)
	return
}

func unlist3R(o Any) (a Any, b Any, c Any, r Any) {
	a = Car(o)
	r = Cdr(o)
	b = Car(r)
	r = Cdr(r)
	c = Car(r)
	r = Cdr(r)
	return
}

func unlist0O(o Any, d Any) Any {
    if _pairZS(o) {
        return Car(o)
    }
	return d
}

func unlist1O(o Any, d Any) (a Any, r Any) {
	a = Car(o)
	r = Cdr(o)
	r = unlist0O(r, d)
	return
}

func unlist2O(o Any, d Any) (a Any, b Any, r Any) {
	a = Car(o)
	r = Cdr(o)
	b = Car(r)
	r = Cdr(r)
	r = unlist0O(r, d)
	return
}

func unlist3O(o Any, d Any) (a Any, b Any, c Any, r Any) {
	a = Car(o)
	r = Cdr(o)
	b = Car(r)
	r = Cdr(r)
	c = Car(r)
	r = Cdr(r)
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

