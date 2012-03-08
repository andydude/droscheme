package droscheme

import ()

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
	NumberTypeCodeI8 = iota
	NumberTypeCodeI16
	NumberTypeCodeI32
	NumberTypeCodeI64
	NumberTypeCodeU8
	NumberTypeCodeU16
	NumberTypeCodeU32
	NumberTypeCodeU64
	NumberTypeCodeExactF32
	NumberTypeCodeExactF64
	NumberTypeCodeExactC64
	NumberTypeCodeExactC128

	// abstract numbers are exact by default
	NumberTypeCodeNatural  // bignat?
	NumberTypeCodeInteger  // bigint?
	NumberTypeCodeRational // [2]integer
	NumberTypeCodeReal     // TBD: func(int)int?

	NumberTypeCodeMax // maximum

	NumberTypeCodeInexact = 0x100
	NumberTypeCodeComplex = 0x200
	NumberTypeCodeQuat    = 0x400

	// machine size number types are exact by default
	NumberTypeCodeInexactI8  = NumberTypeCodeInexact | NumberTypeCodeI8
	NumberTypeCodeInexactI16 = NumberTypeCodeInexact | NumberTypeCodeI16
	NumberTypeCodeInexactI32 = NumberTypeCodeInexact | NumberTypeCodeI32
	NumberTypeCodeInexactI64 = NumberTypeCodeInexact | NumberTypeCodeI64
	NumberTypeCodeInexactU8  = NumberTypeCodeInexact | NumberTypeCodeU8
	NumberTypeCodeInexactU16 = NumberTypeCodeInexact | NumberTypeCodeU16
	NumberTypeCodeInexactU32 = NumberTypeCodeInexact | NumberTypeCodeU32
	NumberTypeCodeInexactU64 = NumberTypeCodeInexact | NumberTypeCodeU64

	// floating point types are inexact by default
	NumberTypeCodeF32  = NumberTypeCodeInexact | NumberTypeCodeExactF32
	NumberTypeCodeF64  = NumberTypeCodeInexact | NumberTypeCodeExactF64
	NumberTypeCodeC64  = NumberTypeCodeInexact | NumberTypeCodeExactC64
	NumberTypeCodeC128 = NumberTypeCodeInexact | NumberTypeCodeExactC128
)

// interfaces
//
// Any - abstracts all data
// Port - abstracts binary/textual/input/output
// Number - abstracts byte/fixnum/bignum/real/rational/complex
// Record - abstracts record types

type Any interface {
	GetType() int
	GetHash() uintptr
	Equal(Any) bool
}

func IsType(o Any, tag int) bool {
	return o.GetType() == tag
}

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
	ToI64() int64
	ToF64() float64
	FromI64(int64) Num
	FromF64(float64) Num
	Cmp1(Num) int // -1, 0, 1
	Add1(Num) Num
	Sub1(Num) Num
	Mul1(Num) Num
	Div1(Num) Num
	Mod1(Num) Num // RTE
	Shl1(Num) Num
	Shr1(Num) Num
}

func Cmp2(x Num, y Num) int { return x.Cmp1(y) }
func Add2(x Num, y Num) Num { return x.Add1(y) }
func Sub2(x Num, y Num) Num { return x.Sub1(y) }
func Mul2(x Num, y Num) Num { return x.Mul1(y) }
func Div2(x Num, y Num) Num { return x.Div1(y) }
func Mod2(x Num, y Num) Num { return x.Mod1(y) }
