package droscheme

import (
	"fmt"
	"math/big"
	"reflect"
)

// exact
type Sint8 int8
type Sint16 int16
type Sint32 int32
type Sint64 int64 // fixnum
type Suint8 uint8
type Suint16 uint16
type Suint32 uint32
type Suint64 uint64
type SExactfloat32 float32
type SExactfloat64 float64
type SExactcomplex64 complex64
type SExactcomplex128 complex128

// inexact
type SInexactint8 int8
type SInexactint16 int16
type SInexactint32 int32
type SInexactint64 int64
type SInexactuint8 uint8
type SInexactuint16 uint16
type SInexactuint32 uint32
type SInexactuint64 uint64
type Sfloat32 float32
type Sfloat64 float64 // flonum
type Scomplex64 complex64
type Scomplex128 complex128

// exact
type SInteger struct { it *big.Int }
type SRational struct { it *big.Rat }

// (in)exact
type SComplex [2]Num
type SComplexPolar [2]Num

func IsInteger(a Any) bool {
	if !IsNumber(a) { return false }
	switch a.(Num).GetNumberType() &^ NumberTypeCodeInexact {
	case NumberTypeCodeI8:
		return true
	case NumberTypeCodeI16:
		return true
	case NumberTypeCodeI32:
		return true
	case NumberTypeCodeI64:
		return true
	case NumberTypeCodeInteger:
		return true
	}
	return false
}

func IsRational(a Any) bool {
	if !IsNumber(a) { return false }
	switch a.(Num).GetNumberType() &^ NumberTypeCodeInexact {
	case NumberTypeCodeRational:
		return true
	}
	return false
}

func IsReal(a Any) bool {
	if !IsNumber(a) { return false }
	if a.(Num).GetNumberType() & NumberTypeCodeComplex != 0 {
		return false
	}
	return true
}

func IsComplex(a Any) bool {
	if !IsNumber(a) { return false }
	if a.(Num).GetNumberType() & NumberTypeCodeComplex != 0 {
		return true
	}
	return false
}

func ToFixnum(a Any) int64 {
	return reflect.ValueOf(a).Int()
}

func ToFlonum(a Any) float64 {
	return reflect.ValueOf(a).Float()
}

func ToFcmplx(a Any) complex128 {
	return reflect.ValueOf(a).Complex()
}

func NewInteger(n int64) Num {
	return SInteger{it: big.NewInt(n)}
}

func NewRational(n, d int64) Num {
	return SRational{it: big.NewRat(n, d)}
}

// type codes
func (o Sint8) GetType() int { return TypeCodeNumber }
func (o Sint8) GetNumberType() int { return NumberTypeCodeI8 }
func (o Sint16) GetType() int { return TypeCodeNumber }
func (o Sint16) GetNumberType() int { return NumberTypeCodeI16 }
func (o Sint32) GetType() int { return TypeCodeNumber }
func (o Sint32) GetNumberType() int { return NumberTypeCodeI32 }
func (o Sint64) GetType() int { return TypeCodeNumber }
func (o Sint64) GetNumberType() int { return NumberTypeCodeI64 }
func (o Suint8) GetType() int { return TypeCodeNumber }
func (o Suint8) GetNumberType() int { return NumberTypeCodeU8 }
func (o Suint16) GetType() int { return TypeCodeNumber }
func (o Suint16) GetNumberType() int { return NumberTypeCodeU16 }
func (o Suint32) GetType() int { return TypeCodeNumber }
func (o Suint32) GetNumberType() int { return NumberTypeCodeU32 }
func (o Suint64) GetType() int { return TypeCodeNumber }
func (o Suint64) GetNumberType() int { return NumberTypeCodeU64 }
func (o Sfloat32) GetType() int { return TypeCodeNumber }
func (o Sfloat32) GetNumberType() int { return NumberTypeCodeF32 }
func (o Sfloat64) GetType() int { return TypeCodeNumber }
func (o Sfloat64) GetNumberType() int { return NumberTypeCodeF64 }
func (o Scomplex64) GetType() int { return TypeCodeNumber }
func (o Scomplex64) GetNumberType() int { return NumberTypeCodeC64 }
func (o Scomplex128) GetType() int { return TypeCodeNumber }
func (o Scomplex128) GetNumberType() int { return NumberTypeCodeC128 }
func (o SInexactint8) GetType() int { return TypeCodeNumber }
func (o SInexactint8) GetNumberType() int { return NumberTypeCodeInexactI8 }
func (o SInexactint16) GetType() int { return TypeCodeNumber }
func (o SInexactint16) GetNumberType() int { return NumberTypeCodeInexactI16 }
func (o SInexactint32) GetType() int { return TypeCodeNumber }
func (o SInexactint32) GetNumberType() int { return NumberTypeCodeInexactI32 }
func (o SInexactint64) GetType() int { return TypeCodeNumber }
func (o SInexactint64) GetNumberType() int { return NumberTypeCodeInexactI64 }
func (o SInexactuint8) GetType() int { return TypeCodeNumber }
func (o SInexactuint8) GetNumberType() int { return NumberTypeCodeInexactU8 }
func (o SInexactuint16) GetType() int { return TypeCodeNumber }
func (o SInexactuint16) GetNumberType() int { return NumberTypeCodeInexactU16 }
func (o SInexactuint32) GetType() int { return TypeCodeNumber }
func (o SInexactuint32) GetNumberType() int { return NumberTypeCodeInexactU32 }
func (o SInexactuint64) GetType() int { return TypeCodeNumber }
func (o SInexactuint64) GetNumberType() int { return NumberTypeCodeInexactU64 }
func (o SExactfloat32) GetType() int { return TypeCodeNumber }
func (o SExactfloat32) GetNumberType() int { return NumberTypeCodeExactF32 }
func (o SExactfloat64) GetType() int { return TypeCodeNumber }
func (o SExactfloat64) GetNumberType() int { return NumberTypeCodeExactF64 }
func (o SExactcomplex64) GetType() int { return TypeCodeNumber }
func (o SExactcomplex64) GetNumberType() int { return NumberTypeCodeExactC64 }
func (o SExactcomplex128) GetType() int { return TypeCodeNumber }
func (o SExactcomplex128) GetNumberType() int { return NumberTypeCodeExactC128 }
func (o SInteger) GetType() int { return TypeCodeNumber }
func (o SInteger) GetNumberType() int { return NumberTypeCodeInteger }
func (o SRational) GetType() int { return TypeCodeNumber }
func (o SRational) GetNumberType() int { return NumberTypeCodeRational }
func (o SComplex) GetType() int { return TypeCodeNumber }
func (o SComplex) GetNumberType() int { return NumberTypeCodeComplex }
func (o SComplexPolar) GetType() int { return TypeCodeNumber }
func (o SComplexPolar) GetNumberType() int { return NumberTypeCodeComplex }

// S32

func (o Sint32) String() string {
	return fmt.Sprintf("%d", o)
}
func (o Sint32) Equal(n Any) bool { return o == n.(Sint32) }
func (o Sint32) Cmp1(n Num) int {
	if o < n.(Sint32) {
		return -1
	}
	if o > n.(Sint32) {
		return 1
	}
	return 0
}
func (o Sint32) Add1(n Num) Num { return Sint32(o + n.(Sint32)) }
func (o Sint32) Sub1(n Num) Num { return Sint32(o - n.(Sint32)) }
func (o Sint32) Mul1(n Num) Num { return Sint32(o * n.(Sint32)) }
func (o Sint32) Div1(n Num) Num { return Sint32(o / n.(Sint32)) }
func (o Sint32) Mod1(n Num) Num { return Sint32(o % n.(Sint32)) }
func (o Sint32) Shl1(n Num) Num { return Sint32(o << uint(n.(Sint32))) }
func (o Sint32) Shr1(n Num) Num { return Sint32(o >> uint(n.(Sint32))) }

// S64

func (o Sint64) String() string {
	return fmt.Sprintf("%d", o)
}
func (o Sint64) Equal(n Any) bool { return o == n.(Sint64) }
func (o Sint64) Cmp1(n Num) int {
	if o < n.(Sint64) { return -1 }
	if o > n.(Sint64) { return 1 }
	return 0
}
func (o Sint64) Add1(n Num) Num { return Sint64(o + n.(Sint64)) }
func (o Sint64) Sub1(n Num) Num { return Sint64(o - n.(Sint64)) }
func (o Sint64) Mul1(n Num) Num { return Sint64(o * n.(Sint64)) }
func (o Sint64) Div1(n Num) Num { return Sint64(o / n.(Sint64)) }
func (o Sint64) Mod1(n Num) Num { return Sint64(o % n.(Sint64)) }
func (o Sint64) Shl1(n Num) Num { return Sint64(o << uint(n.(Sint64))) }
func (o Sint64) Shr1(n Num) Num { return Sint64(o >> uint(n.(Sint64))) }

//// U32
//func (o Suint32) Add1(n Num) Num { return Suint32(o + n.(Suint32)) }
//func (o Suint32) Sub1(n Num) Num { return Suint32(o - n.(Suint32)) }
//func (o Suint32) Mul1(n Num) Num { return Suint32(o * n.(Suint32)) }
//func (o Suint32) Div1(n Num) Num { return Suint32(o / n.(Suint32)) }
//func (o Suint32) Mod1(n Num) Num { return Suint32(o % n.(Suint32)) }
//func (o Suint32) Shl1(n Num) Num { return Suint32(o << n.(Suint32)) }
//func (o Suint32) Shr1(n Num) Num { return Suint32(o >> n.(Suint32)) }
//
//// U64
//func (o Suint64) Add1(n Num) Num { return Suint64(o + n.(Suint64)) }
//func (o Suint64) Sub1(n Num) Num { return Suint64(o - n.(Suint64)) }
//func (o Suint64) Mul1(n Num) Num { return Suint64(o * n.(Suint64)) }
//func (o Suint64) Div1(n Num) Num { return Suint64(o / n.(Suint64)) }
//func (o Suint64) Mod1(n Num) Num { return Suint64(o % n.(Suint64)) }
//func (o Suint64) Shl1(n Num) Num { return Suint64(o << n.(Suint64)) }
//func (o Suint64) Shr1(n Num) Num { return Suint64(o >> n.(Suint64)) }

// F32

func (o Sfloat32) Equal(n Any) bool { return o == n.(Sfloat32) }
func (o Sfloat32) Cmp1(n Num) int {
	if o < n.(Sfloat32) { return -1 }
	if o > n.(Sfloat32) { return 1 }
	return 0
}
func (o Sfloat32) Add1(n Num) Num { return Sfloat32(o + n.(Sfloat32)) }
func (o Sfloat32) Sub1(n Num) Num { return Sfloat32(o - n.(Sfloat32)) }
func (o Sfloat32) Mul1(n Num) Num { return Sfloat32(o * n.(Sfloat32)) }
func (o Sfloat32) Div1(n Num) Num { return Sfloat32(o / n.(Sfloat32)) }
func (o Sfloat32) Mod1(n Num) Num { return Sfloat32(0) } // wrong
func (o Sfloat32) Shl1(n Num) Num { return Sfloat32(0) } // wrong
func (o Sfloat32) Shr1(n Num) Num { return Sfloat32(0) } // wrong

// F64

func (o Sfloat64) Equal(n Any) bool { return o == n.(Sfloat64) }
func (o Sfloat64) Cmp1(n Num) int {
	if o < n.(Sfloat64) { return -1 }
	if o > n.(Sfloat64) { return 1 }
	return 0
}
func (o Sfloat64) Add1(n Num) Num { return Sfloat64(o + n.(Sfloat64)) }
func (o Sfloat64) Sub1(n Num) Num { return Sfloat64(o - n.(Sfloat64)) }
func (o Sfloat64) Mul1(n Num) Num { return Sfloat64(o * n.(Sfloat64)) }
func (o Sfloat64) Div1(n Num) Num { return Sfloat64(o / n.(Sfloat64)) }
func (o Sfloat64) Mod1(n Num) Num { return Sfloat64(0) } // wrong
func (o Sfloat64) Shl1(n Num) Num { return Sfloat64(0) } // wrong
func (o Sfloat64) Shr1(n Num) Num { return Sfloat64(0) } // wrong

//func (o Scomplex64) Add1(n Num) Num { return Scomplex64(o + n.(Scomplex64)) }
//func (o Scomplex64) Sub1(n Num) Num { return Scomplex64(o - n.(Scomplex64)) }
//func (o Scomplex64) Mul1(n Num) Num { return Scomplex64(o * n.(Scomplex64)) }
//func (o Scomplex64) Div1(n Num) Num { return Scomplex64(o / n.(Scomplex64)) }
//func (o Scomplex64) Mod1(n Num) Num { return Scomplex64(o % n.(Scomplex64)) }
//func (o Scomplex64) Shl1(n Num) Num { return Scomplex64(o << n.(Scomplex64)) } // wrong
//func (o Scomplex64) Shr1(n Num) Num { return Scomplex64(o >> n.(Scomplex64)) } // wrong
//func (o Scomplex128) Add1(n Num) Num { return Scomplex128(o + n.(Scomplex128)) }
//func (o Scomplex128) Sub1(n Num) Num { return Scomplex128(o - n.(Scomplex128)) }
//func (o Scomplex128) Mul1(n Num) Num { return Scomplex128(o * n.(Scomplex128)) }
//func (o Scomplex128) Div1(n Num) Num { return Scomplex128(o / n.(Scomplex128)) }
//func (o Scomplex128) Mod1(n Num) Num { return Scomplex128(o % n.(Scomplex128)) }
//func (o Scomplex128) Shl1(n Num) Num { return Scomplex128(o << n.(Scomplex128)) } // wrong
//func (o Scomplex128) Shr1(n Num) Num { return Scomplex128(o >> n.(Scomplex128)) } // wrong

// Integer

func (o SInteger) String() string {
	return o.it.String()
}
func (o SInteger) Equal(n Any) bool { return o.Cmp1(n.(SInteger)) == 0 }
func (o SInteger) Cmp1(n Num) int { return o.it.Cmp(n.(SInteger).it) }
func (o SInteger) Add1(n Num) Num { 
	return SInteger{it: big.NewInt(0).Add(o.it, n.(SInteger).it)}
}
func (o SInteger) Sub1(n Num) Num { 
	return SInteger{it: big.NewInt(0).Sub(o.it, n.(SInteger).it)}
}
func (o SInteger) Mul1(n Num) Num { 
	return SInteger{it: big.NewInt(0).Mul(o.it, n.(SInteger).it)}
}
func (o SInteger) Div1(n Num) Num { 
	return SInteger{it: big.NewInt(0).Div(o.it, n.(SInteger).it)}
}
func (o SInteger) Mod1(n Num) Num { 
	return SInteger{it: big.NewInt(0).Mod(o.it, n.(SInteger).it)}
}
func (o SInteger) Shl1(n Num) Num {
	return SInteger{it: big.NewInt(0).Rsh(o.it, uint(ToFixnum(n)))}
}
func (o SInteger) Shr1(n Num) Num {
	return SInteger{it: big.NewInt(0).Lsh(o.it, uint(ToFixnum(n)))}
}

// Rational

func (o SRational) String() string {
	return o.it.RatString()
}
func (o SRational) Equal(n Any) bool {
	return o.Cmp1(n.(SRational)) == 0
}
func (o SRational) Cmp1(n Num) int {
	return o.it.Cmp(n.(SRational).it)
}
func (o SRational) Add1(n Num) Num {
	return SRational{it: big.NewRat(0, 1).Add(o.it, n.(SRational).it)}
}
func (o SRational) Sub1(n Num) Num {
	return SRational{it: big.NewRat(0, 1).Sub(o.it, n.(SRational).it)}
}
func (o SRational) Mul1(n Num) Num { 
	return SRational{it: big.NewRat(0, 1).Mul(o.it, n.(SRational).it)}
}
func (o SRational) Div1(n Num) Num { return Sfloat64(0) } // wrong
func (o SRational) Mod1(n Num) Num { return Sfloat64(0) } // wrong
func (o SRational) Shl1(n Num) Num { return Sfloat64(0) } // wrong
func (o SRational) Shr1(n Num) Num { return Sfloat64(0) } // wrong

// Complex

func (o SComplex) String() string {
	return fmt.Sprintf("%s+%si", o[0], o[1])
}
func (o SComplex) Equal(n Any) bool { return Equal(o, n) }
func (o SComplex) Cmp1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplex) Add1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplex) Sub1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplex) Mul1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplex) Div1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplex) Mod1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplex) Shl1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplex) Shr1(n Num) Num { return Sfloat64(0) } // wrong

// ComplexPolar

func (o SComplexPolar) String() string {
	return fmt.Sprintf("%s@%s", o[0], o[1])
}
func (o SComplexPolar) Equal(n Any) bool { return Equal(o, n) }
func (o SComplexPolar) Cmp1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplexPolar) Add1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplexPolar) Sub1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplexPolar) Mul1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplexPolar) Div1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplexPolar) Mod1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplexPolar) Shl1(n Num) Num { return Sfloat64(0) } // wrong
func (o SComplexPolar) Shr1(n Num) Num { return Sfloat64(0) } // wrong
