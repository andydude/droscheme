package droscheme

import (
	"fmt"
	"errors"
)

// structures and methods in this file
//
// SBool
// SChar
// SNull
// SPair
// SVector
// SBinary
// SString
// SSymbol
// SType
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
	return fmt.Sprintf("#\\x%X", int(o))
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

// s:pair type

type SPair struct {
	car Any
	cdr Any
}

// s:pair methods

func IsPair(o Any) bool {
	var _, ok = o.(SPair)
	return ok
}

func (o SPair) GetType() int {
	return TypeCodePair
}

func (o SPair) GetHash() uintptr {
	return 0 // TODO
}

func (o SPair) Equal(a Any) bool {
	return false // TODO
}

func (o SPair) String() string {
	return fmt.Sprintf("(%s . %s)", o.car, o.cdr)
}

func IsList(o Any) bool {
	// By definition, all lists are chains of pairs that have 
	// finite length and are terminated by the empty list. [R6RS]

	// cycle detection (only needed in mutable model)
	switch o.GetType() {
	case TypeCodeNull:
		return true
	case TypeCodePair:
		return IsList(o.(SPair).cdr)
	}
	return false
}

func Length(o Any) int {
	// cycle detection (only needed in mutable model)
	switch o.GetType() {
	case TypeCodePair:
		return 1 + Length(o.(SPair).cdr)
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

func (o SSymbol) GetType() int {
	return TypeCodeSymbol
}

func (o SSymbol) Equal(a Any) bool {
	return o.name == a.(SSymbol).name
}

func (o SSymbol) String() string {
	return o.name
}

// vector type

type SVector struct {
	items    []Any
	itemtype int
}

func IsVector(a Any) bool {
	return IsType(a, TypeCodeVector)
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

func (o SVector) String() string {
	if len(o.items) == 0 {
		return "#()"
	}

	var ret string = ""
	for i := 0; i < len(o.items); i++ {
		ret += fmt.Sprintf(" %s", o.items[i])
	}
	return fmt.Sprintf("#(%s)", ret[1:])
}

// syntax type

type SSyntax struct {
	form func(Any, *Env) Any
	name string
}

// syntax methods

func (o SSyntax) GetType() int {
	return TypeCodeSyntax
}

func (o SSyntax) Equal(a Any) bool {
	return false
}

// procedure type

type SProc struct {
	call func(Any)Any
	name string
}

// procedure methods

func IsProcedure(o Any) bool {
	return IsType(o, TypeCodeProc)
}

func (o SProc) GetType() int {
	return TypeCodeProc
}

func (o SProc) GetHash() uintptr {
	return 0 // TODO
}

func (o SProc) Equal(a Any) bool {
	return false
}

func (o SProc) String() string {
	return fmt.Sprintf("#<procedure:%s>", o.name)
}

// values type

type SValues struct {
	values SVector
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
	if len(o.values.items) == 0 {
		return ""
	}
	return fmt.Sprintf("#<values:%s>", o.values)
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
	return SPair{a, SNull{}}
}

func list2(a, b Any) Any {
	return SPair{a, SPair{b, SNull{}}}
}

func list3(a, b, c Any) Any {
	return SPair{a, SPair{b, SPair{c, SNull{}}}}
}

func list1R(a, rest Any) Any {
	return SPair{a, rest}
}

func list2R(a, b, rest Any) Any {
	return SPair{a, SPair{b, rest}}
}

func list3R(a, b, c, rest Any) Any {
	return SPair{a, SPair{b, SPair{c, rest}}}
}

func listR(most, last Any) Any {
	//// this would have worked in the mutable model
	//var lastpair Any
	//for lastpair = most;
	//IsPair(lastpair.(SPair).cdr);
	//lastpair = lastpair.(SPair).cdr {}
	//lastpair.(SPair).cdr = last
	//return most

	// immutable model requires reconstruction
	if IsPair(most) {
		return SPair{most.(SPair).car,
			listR(most.(SPair).cdr, last)}
	}

	// assume IsNull
	return last
}

func unlist1(o Any) Any {
	return o.(SPair).car
}

func unlist2(o Any) (a, b Any) {
	a = o.(SPair).car
	b = o.(SPair).cdr
	b = b.(SPair).car
	return
}

func unlist3(o Any) (a, b, c Any) {
	a = o.(SPair).car
	c = o.(SPair).cdr
	b = c.(SPair).car
	c = c.(SPair).cdr
	c = c.(SPair).car
	return
}

func unlist1R(o Any) (a Any, r Any) {
	a = o.(SPair).car
	r = o.(SPair).cdr
	return
}

func unlist2R(o Any) (a Any, b Any, r Any) {
	a = o.(SPair).car
	r = o.(SPair).cdr
	b = r.(SPair).car
	r = r.(SPair).cdr
	return
}

func unlist3R(o Any) (a Any, b Any, c Any, r Any) {
	a = o.(SPair).car
	r = o.(SPair).cdr
	b = r.(SPair).car
	r = r.(SPair).cdr
	c = r.(SPair).car
	r = r.(SPair).cdr
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
	return SValues{values: SVector{items: []Any{}}}
}

// represents 2 return values
func values2(a, b Any) Any {
	return SValues{values: SVector{items: []Any{a, b}}}
}

// represents multiple return values
func valuesR(rest Any) Any {
    vec := DlistZKZRvector(list1(rest)).(SVector)
    return SValues{values: vec}
}
