package droscheme

import (
	"reflect"
)

const (
	TypeCodeAny = iota // reserved
	TypeCodeType    // go:SType
	TypeCodeNull    // go:SNull     s:null?
	TypeCodePair    // go:SPair     s:pair?
	TypeCodeChar    // go:SChar     s:char?
	TypeCodeBool    // go:SBool     s:boolean?
	TypeCodeProc    // go:SProc     s:procedure?
	TypeCodeBinary  // go:SBinary   s:bytevector?
	TypeCodeNumber  // go:Num       s:number?     -- interface
	TypeCodePort    // go:Port      s:port?       -- interface
	TypeCodeString  // go:SString   s:string?
	TypeCodeSymbol  // go:SSymbol   s:symbol?
	TypeCodeVector  // go:SVector   s:vector?
	TypeCodeRecord  // go:Record                  -- interface
	TypeCodeLibrary //
	TypeCodeValues  // multiple return values

	// ... we can add more nonstandard types later

	TypeCodeMax // maximum
)

const (
	PortTypeCodeByte      = iota
	PortTypeCodeByteIn    // binary intput port
	PortTypeCodeByteOut   // binary output port
	PortTypeCodeByteInOut // binary port

	PortTypeCodeChar
	PortTypeCodeCharIn    // textual input port
	PortTypeCodeCharOut   // textual output port
	PortTypeCodeCharInOut // textual port

	PortTypeCodeAny
	PortTypeCodeAnyIn    // nonstandard, <-chan Any
	PortTypeCodeAnyOut   // nonstandard, chan<- Any
	PortTypeCodeAnyInOut // nonstandard, chan Any

	PortTypeCodeMax // maximum
)

const (
	// machine size numbers
	NumberTypeCodeS8 = iota
	NumberTypeCodeS16
	NumberTypeCodeS32
	NumberTypeCodeS64
	NumberTypeCodeExactF32
	NumberTypeCodeExactF64

	// abstract numbers are exact by default
	NumberTypeCodeInteger  // bigint?
	NumberTypeCodeRational // bigrat?
	NumberTypeCodeReal     // TBD: func(int)int?
	NumberTypeCodeBaseMax  // maximum

	// general number bit field
	NumberTypeCodeMask     = 0xF
	NumberTypeCodeComplex  = 0x10
	NumberTypeCodeUnsigned = 0x20
	NumberTypeCodeCompolar = 0x30
	NumberTypeCodeInexact  = 0x40
	NumberTypeCodeReserved = 0x80

	NumberTypeCodeU8  = NumberTypeCodeUnsigned | NumberTypeCodeS8
	NumberTypeCodeU16 = NumberTypeCodeUnsigned | NumberTypeCodeS16
	NumberTypeCodeU32 = NumberTypeCodeUnsigned | NumberTypeCodeS32
	NumberTypeCodeU64 = NumberTypeCodeUnsigned | NumberTypeCodeS64
	NumberTypeCodeNatural = NumberTypeCodeUnsigned | NumberTypeCodeInteger
	NumberTypeCodeExactC64  = NumberTypeCodeComplex | NumberTypeCodeExactF32
	NumberTypeCodeExactC128 = NumberTypeCodeComplex | NumberTypeCodeExactF64
	NumberTypeCodeInexactS8  = NumberTypeCodeInexact | NumberTypeCodeS8
	NumberTypeCodeInexactS16 = NumberTypeCodeInexact | NumberTypeCodeS16
	NumberTypeCodeInexactS32 = NumberTypeCodeInexact | NumberTypeCodeS32
	NumberTypeCodeInexactS64 = NumberTypeCodeInexact | NumberTypeCodeS64
	NumberTypeCodeInexactU8  = NumberTypeCodeInexact | NumberTypeCodeU8
	NumberTypeCodeInexactU16 = NumberTypeCodeInexact | NumberTypeCodeU16
	NumberTypeCodeInexactU32 = NumberTypeCodeInexact | NumberTypeCodeU32
	NumberTypeCodeInexactU64 = NumberTypeCodeInexact | NumberTypeCodeU64
	NumberTypeCodeF32  = NumberTypeCodeInexact | NumberTypeCodeExactF32
	NumberTypeCodeF64  = NumberTypeCodeInexact | NumberTypeCodeExactF64
	NumberTypeCodeC64  = NumberTypeCodeInexact | NumberTypeCodeExactC64
	NumberTypeCodeC128 = NumberTypeCodeInexact | NumberTypeCodeExactC128

	NumberTypeCodeMax
)

// interfaces
//
// Any - abstracts all data
// Port - abstracts binary/textual/input/output
// Number - abstracts byte/fixnum/bignum/real/rational/complex
// Record - abstracts record types

type Any interface {
	GetType() int
	Equal(Any) bool
}

func IsType(o Any, tag int) bool {
	return o.GetType() == tag
}

func Equal(x, y Any) bool {
	return reflect.DeepEqual(x, y)
}

func Hash(o Any) uintptr {
	return reflect.ValueOf(&o).Pointer()
}

// testing

// TODO: make a table of STypes with type names etc.
//GetTypeName() string can come form table
//GetTypeInfo() Any can come from table

//type List interface {
//	Any
//	//GetCar() Any
//	//GetCdr() (Any, os.Error)
//	//SetCar(Any) Any
//	//SetCdr(Any) (Any, os.Error)
//	//GetCar(int) (Any, os.Error)
//	//GetCdr(int) (Any, os.Error)
//	//GetLength() int // recursive
//	//GetElementType() Type
//}

// s:port?

type Port interface {
	Any
	GetPortType() int
	Read() Any
	Write(Any)
}

func IsPort(o Any) bool {
	var _, ok = o.(Port)
	return ok
}

func IsBinaryPort(o Any) bool {
	var p, ok = o.(Port)
	if !ok { return false }
	var t = p.GetPortType()
	if t > PortTypeCodeByteInOut { return false }
	return true
}

func IsTextualPort(o Any) bool {
	var p, ok = o.(Port)
	if !ok { return false }
	var t = p.GetPortType()
	if t > PortTypeCodeCharInOut { return false }
	if t < PortTypeCodeChar { return false }
	return true
}

func IsInputPort(o Any) bool {
	var p, ok = o.(Port)
	if !ok { return false }
	var t = p.GetPortType()
	if t & PortTypeCodeByteIn == 0 { return false }
	return true
}

func IsOutputPort(o Any) bool {
	var p, ok = o.(Port)
	if !ok { return false }
	var t = p.GetPortType()
	if t & PortTypeCodeByteOut == 0 { return false }
	return true
}

// s:number?

type Num interface {
	Any
	GetNumberType() int
	//FromFixnum(int64) Num
	//FromFlonum(float64) Num
	Cmp1(Num) int // -1, 0, 1
	Add1(Num) Num
	Sub1(Num) Num
	Mul1(Num) Num
	Div1(Num) Num
	Mod1(Num) Num // RTE
	Shl1(Num) Num
	Shr1(Num) Num
}

func IsNumber(a Any) bool {
	return IsType(a, TypeCodeNumber)
}

func Cmp2(x Num, y Num) int { return x.Cmp1(y) }
func Add2(x Num, y Num) Num { return x.Add1(y) }
func Sub2(x Num, y Num) Num { return x.Sub1(y) }
func Mul2(x Num, y Num) Num { return x.Mul1(y) }
func Div2(x Num, y Num) Num { return x.Div1(y) }
func Mod2(x Num, y Num) Num { return x.Mod1(y) }
