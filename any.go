package droscheme

import (
	"os"
)

const (
	TypeCodeNull = iota
	TypeCodeType    // go:AnyType
	TypeCodePair    // go:AnyPair   s:pair?
	TypeCodeBool    // go:bool	    s:boolean?
	TypeCodeProc    // go:AnyFunc   s:procedure?
	TypeCodeBinary  // go:AnyBinary s:bytevector?
	TypeCodeNumber  // go:AnyNumber s:number?
	TypeCodePort    // go:AnyStream s:port?
	TypeCodeString  // go:string	s:string?
	TypeCodeSymbol  // go:AnySymbol s:symbol?
	TypeCodeVector  // go:AnyVector s:vector?
	TypeCodeChar	// go:uint32	s:char?
	TypeCodeByte    // go:byte
	TypeCodeRecord  // go:AnyRecord
	TypeCodeLibrary // go:AnyModule
	TypeCodeUintptr // go:uintptr

	// ... we can add more nonstandard types later

	TypeCodeMax // maximum
)

const (
	PortTypeCodeByte = iota
	PortTypeCodeByteIn    // binary intput port
	PortTypeCodeByteOut	  // binary output port
	PortTypeCodeByteInOut // binary port
	PortTypeCodeChar
	PortTypeCodeCharIn	  // textual input port
	PortTypeCodeCharOut	  // textual output port
	PortTypeCodeCharInOut // textual port
	PortTypeCodeAny
	PortTypeCodeAnyIn     // nonstandard, <-chan Any
	PortTypeCodeAnyOut	  // nonstandard, chan<- Any
	PortTypeCodeAnyInOut  // nonstandard, chan Any

	PortTypeCodeMax // maximum
)

const (
	NumberTypeCodeUnknown = iota
	NumberTypeCodeNatural  // uint64
	NumberTypeCodeInteger  // int64
	NumberTypeCodeFloat    // float64
	NumberTypeCodeRational // []int64
	NumberTypeCodeReal     // TBD: func(int)int?

	NumberTypeCodeMax // maximum

	NumberTypeCodeInexact = 0x100
	NumberTypeCodeComplex = 0x200
	NumberTypeCodeQuat    = 0x400
)

// minimal types

//type SType struct {
//	Code int
//	Name string
//}

// interfaces
//
// Any - abstracts all data
// List - abstracts null/pair
// Port - abstracts binary/textual/input/output
// Number - abstracts byte/fixnum/bignum/real/rational/complex
// Record - abstracts record types

type Any interface {
	GetType() int
	GetTypeName() string
	GetTypeInfo() Any
	Equal(Any) bool
    Hash() uintptr
}

func IsType(o Any, tag int) bool {
	return o.GetType() == tag
}

type List interface {
	Any
	//GetCar() Any
	//GetCdr() (Any, os.Error)
	//SetCar(Any) Any
	//SetCdr(Any) (Any, os.Error)
	GetCarAt(int) (Any, os.Error)
	GetCdrAt(int) (Any, os.Error)
	GetLength() int // recursive
	IsProper() bool // recursive
	//GetElementType() Type
}

func IsList(o Any) bool {
	var _, ok = o.(List)
	return ok
}

func IsNull(o Any) bool {
	return IsType(o, TypeCodeNull)
}

func IsPair(o Any) bool {
	return IsType(o, TypeCodePair)
}

type Port interface {
	Any
	GetPortType() int
	Read()Any
	Write(Any)
}

func IsPort(o Any) bool {
	var _, ok = o.(Port)
	return ok
}

func IsBinaryPort(o Any) bool {
	if !IsPort(o) {
		return false
	}
	var p, ok = o.(Port)
	if !ok { return false }

	 switch p.GetPortType() {
	 case PortTypeCodeByteIn:
		 fallthrough
	 case PortTypeCodeByteOut:
		 fallthrough
	 case PortTypeCodeByteInOut:
		 return true
	 }

	 return false
 }

func IsTextualPort(o Any) bool
func IsInputPort(o Any) bool
func IsOutputPort(o Any) bool

type Num interface {
	Any
	GetNumberType() int
	Cmp1(Num) int // -1, 0, 1
	Add1(Num) Num
	Sub1(Num) Num
	Mul1(Num) Num
	Div1(Num) Num
	Mod1(Num) Num // RTE
}

func Cmp2(x Num, y Num) int { return x.Cmp1(y) }
func Add2(x Num, y Num) Num { return x.Add1(y) }
func Sub2(x Num, y Num) Num { return x.Sub1(y) }
func Mul2(x Num, y Num) Num { return x.Mul1(y) }
func Div2(x Num, y Num) Num { return x.Div1(y) }
func Mod2(x Num, y Num) Num { return x.Mod1(y) }

