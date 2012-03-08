package droscheme

// structures and methods in this file
//
// SBool
// SChar
// SNull
// SPair
// SVector
// SBinary
// SString
// SType
// SBytePort
// SCharPort

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

// character type

type SChar int 

// character methods

func IsChar(o Any) bool {
	var _, ok = o.(SChar)
	return ok
}

func (o SChar) GetType() int {
	return TypeCodeChar
}

func (o SChar) GetHash() uintptr {
	return 0 // TODO
}

func (o SChar) Equal(Any) bool {
	return false // TODO
}

// null type

type SNull struct {}

// null methods

func IsNull(o Any) bool {
	var _, ok = o.(SNull)
	return ok
}

// compare with
//func IsNull(o Any) bool {
//	return IsType(o, TypeCodeNull)
//}

func (o SNull) GetType() int {
	return TypeCodeNull
}

func (o SNull) GetHash() uintptr {
	// TODO
	return 0
}

func (_ SNull) Equal(a Any) bool {
	return false // TODO
}

func (_ SNull) Length() int {
	return 0
}

func (_ SNull) IsList() bool {
	// By definition, all lists are chains of pairs that have 
	// finite length and are terminated by the empty list. [R6RS]
	return true
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

// compare with
//func IsPair(o Any) bool {
//	return IsType(o, TypeCodePair)
//}

func (o SPair) GetType() int {
	return TypeCodePair
}

func (o SPair) GetHash() uintptr {
	return 0 // TODO
}

func (o SPair) Equal(a Any) bool {
	return false // TODO
}

func (o SPair) Length() int {
	// TODO: cycle detection
	switch o.cdr.GetType() {
	case TypeCodePair:
		return 1 + o.cdr.(SPair).Length()
	case TypeCodeNull:
		return 1
	}
	return 2
}

func (o SPair) IsList() bool {
	// By definition, all lists are chains of pairs that have 
	// finite length and are terminated by the empty list. [R6RS]

	// TODO: cycle detection
	switch o.cdr.GetType() {
	case TypeCodePair:
		return o.cdr.(SPair).IsList()
	case TypeCodeNull:
		return true
	}
	return false
}

// s:bytevector type

type SBinary struct {
	bytes []byte
}

func (o SBinary) GetType() int {
	return TypeCodeVector
}

func (o SBinary) GetHash() uintptr {
	return 0 // TODO
}

func (o SBinary) Equal(a Any) bool {
	return false // TODO
}

// s:bytevector type

type SString struct {
	text string
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

// s:vector type

type SVector struct {
	items []Any
	itemtype int
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

// misc

type AnyVec []Any
type AnyEnv map[string]Any
type AnyMap map[Any]Any

type AnyMapping interface {
	Any
	Get(Any) Any
	Set(Any, Any)
}

// type type

type SType struct {
	typeName string
	typeCode int
	portTypeCode int
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

// returns multiple values for argument handling
// so I don't think we need to export any of these

func unlist1(a Any) Any {
	return a.(SPair).car
}

func unlist2(a Any) (y Any, z Any) {
	y = a.(SPair).car
	z = a.(SPair).cdr.(SPair).car
	return
}

func unlist3(a Any) (x Any, y Any, z Any) {
	var w = a.(SPair).cdr.(SPair)
	x = a.(SPair).car
	y = w.car
	z = w.cdr.(SPair).car
	return
}

func list1(a Any) Any {
	return SPair{a, SNull{}}
}

func list2(a Any, b Any) Any {
	return SPair{a, SPair{b, SNull{}}}
}

func list3(a Any, b Any, c Any) Any {
	return SPair{a, SPair{b, SPair{c, SNull{}}}}
}
