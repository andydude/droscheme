package droscheme


// structures and methods
//
// SBool
// SChar
// SNull
// SPair
// SBytePort
// SByteIPort
// SByteOPort
// SCharPort
// SCharIPort
// SCharOPort
// SBinary
// SString
// SType


type SType struct {
	typeName string
	typeKind TypeCode
	portKind PortTypeCode
}

func (o SType) GetName() string {
	return o.typeName
}

func (o SType) GetCode() int {
	return o.typeCode
}

type SBool struct {}

func (o SBool) GetName() string {
	return "boolean"
}

func (o SBool) GetCode() TypeCode {
	return TypeCodeBool
}

func (o SBool) GetHash(Any) HashCode {
}

func (o SBool) Equal(Any, Any) bool {
}

func (o SBool) Read(IPort) Any {
}

func (o SBool) Write(Any, IPort) {
}

func (o SBool) WriteDebug(Any) {
	o.Write(Any, stdout)
}





type SNull struct {}

func (_ SNull) Length() int {
	return 0
}

func (_ SNull) IsProper() bool {
	return true
}

func (_ SNull) IsNull() bool {
	return true
}

func (_ SNull) IsPair() bool {
	return false
}

type SPair struct {
	Car Any
	Cdr Any
}

func (o SPair) Length() int {
	switch o.Cdr.GetType().GetCode() {
	case TypeCodePair:
		return 1 + o.Cdr.Length()
	case TypeCodeNull:
		return 1
	default:
		return 2
	}
}

func (o SPair) IsProper() bool {
	switch o.Cdr.GetType().GetCode() {
	case TypeCodePair:
		return o.Cdr.Proper()
	case TypeCodeNull:
		return true
	default:
		return false
	}
}

func (_ SPair) IsNull() bool {
	return false
}

func (_ SPair) IsPair() bool {
	return true
}

type BytePort struct {
	Chan chan byte
	Peek int // -1 indicates no byte
}

func (a BytePort) GetType() Type {
}

type CharPort struct {
	Chan chan int
	Peek int // -1 indicates no char
}

type AnyVec []Any
type AnyEnv map[string]Any
type AnyMap map[Any]Any

type AnyMapping interface {
	Any
	Get(Any) Any
	Set(Any, Any)
}

type AnyNumber interface {
	Any
	Add2(AnyNumber)
	Mul2(AnyNumber)
}

// functions

func (this uintptr) GetType() TypeCode {
	return TypeCodeUintptr
}

func (this uintptr) IsEqual(that uintptr) bool {
	return this == that
}

func (this string) GetType() TypeCode {
	return TypeCodeString
}

func (this string) GetCode(that uintptr) bool {
	return len(this)
}

func (this AnyPair) GetType() TypeCode {
	return TypeCodePair
}

func (this AnyPair) IsEqual(that AnyPair) bool {
	if this != that {
		return false
	}
}


