package droscheme

import (
	"fmt"
)

type Sint8 int8
type Sint16 int16
type Sint32 int32
type Sint64 int64
type Suint8 uint8
type Suint16 uint16
type Suint32 uint32
type Suint64 uint64
type Sfloat32 float32
type Sfloat64 float64
type Scomplex64 complex64
type Scomplex128 complex128

func IsInteger(a Any) bool {
	if !IsNumber(a) { return false }
	switch a.(Num).GetNumberType() &^ NumberTypeCodeInexact {
	case NumberTypeCodeI8:
		fallthrough
	case NumberTypeCodeI16:
		fallthrough
	case NumberTypeCodeI32:
		fallthrough
	case NumberTypeCodeI64:
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

func (o Sint32) GetHash() uintptr { return uintptr(o) }
func (o Sint32) Equal(n Any) bool { return o == n.(Sint32) }
func (o Sint32) ToI64() int64 { return int64(o) }
func (o Sint32) ToF64() float64 { return float64(o) }
func (o Sint32) FromI64(n int64) Num { return Sint32(n) }
func (o Sint32) FromF64(n float64) Num { return Sint32(n) }
func (o Sint32) Cmp1(n Num) int {
	if o < n.(Sint32) {
		return -1
	}
	if o > n.(Sint32) {
		return 1
	}
	return 0
}

func (o Sint64) GetHash() uintptr { return uintptr(o) }
func (o Sint64) Equal(n Any) bool { return o == n.(Sint64) }
func (o Sint64) ToI64() int64 { return int64(o) }
func (o Sint64) ToF64() float64 { return float64(o) }
func (o Sint64) FromI64(n int64) Num { return Sint64(n) }
func (o Sint64) FromF64(n float64) Num { return Sint64(n) }
func (o Sint64) Cmp1(n Num) int {
	if o < n.(Sint64) {
		return -1
	}
	if o > n.(Sint64) {
		return 1
	}
	return 0
}

// boring

func (o Sint64) String() string {
	return fmt.Sprintf("%d", o)
}

func (o Sint32) Add1(n Num) Num { return Sint32(o + n.(Sint32)) }
func (o Sint32) Sub1(n Num) Num { return Sint32(o - n.(Sint32)) }
func (o Sint32) Mul1(n Num) Num { return Sint32(o * n.(Sint32)) }
func (o Sint32) Div1(n Num) Num { return Sint32(o / n.(Sint32)) }
func (o Sint32) Mod1(n Num) Num { return Sint32(o % n.(Sint32)) }
func (o Sint32) Shl1(n Num) Num { return Sint32(o << uint(n.(Sint32))) }
func (o Sint32) Shr1(n Num) Num { return Sint32(o >> uint(n.(Sint32))) }
func (o Sint64) Add1(n Num) Num { return Sint64(o + n.(Sint64)) }
func (o Sint64) Sub1(n Num) Num { return Sint64(o - n.(Sint64)) }
func (o Sint64) Mul1(n Num) Num { return Sint64(o * n.(Sint64)) }
func (o Sint64) Div1(n Num) Num { return Sint64(o / n.(Sint64)) }
func (o Sint64) Mod1(n Num) Num { return Sint64(o % n.(Sint64)) }
func (o Sint64) Shl1(n Num) Num { return Sint64(o << uint(n.(Sint64))) }
func (o Sint64) Shr1(n Num) Num { return Sint64(o >> uint(n.(Sint64))) }

//func (o Suint32) Add1(n Num) Num { return Suint32(o + n.(Suint32)) }
//func (o Suint32) Sub1(n Num) Num { return Suint32(o - n.(Suint32)) }
//func (o Suint32) Mul1(n Num) Num { return Suint32(o * n.(Suint32)) }
//func (o Suint32) Div1(n Num) Num { return Suint32(o / n.(Suint32)) }
//func (o Suint32) Mod1(n Num) Num { return Suint32(o % n.(Suint32)) }
//func (o Suint32) Shl1(n Num) Num { return Suint32(o << n.(Suint32)) }
//func (o Suint32) Shr1(n Num) Num { return Suint32(o >> n.(Suint32)) }
//func (o Suint64) Add1(n Num) Num { return Suint64(o + n.(Suint64)) }
//func (o Suint64) Sub1(n Num) Num { return Suint64(o - n.(Suint64)) }
//func (o Suint64) Mul1(n Num) Num { return Suint64(o * n.(Suint64)) }
//func (o Suint64) Div1(n Num) Num { return Suint64(o / n.(Suint64)) }
//func (o Suint64) Mod1(n Num) Num { return Suint64(o % n.(Suint64)) }
//func (o Suint64) Shl1(n Num) Num { return Suint64(o << n.(Suint64)) }
//func (o Suint64) Shr1(n Num) Num { return Suint64(o >> n.(Suint64)) }
//func (o Sfloat32) Add1(n Num) Num { return Sfloat32(o + n.(Sfloat32)) }
//func (o Sfloat32) Sub1(n Num) Num { return Sfloat32(o - n.(Sfloat32)) }
//func (o Sfloat32) Mul1(n Num) Num { return Sfloat32(o * n.(Sfloat32)) }
//func (o Sfloat32) Div1(n Num) Num { return Sfloat32(o / n.(Sfloat32)) }
//func (o Sfloat32) Mod1(n Num) Num { return Sfloat32(o % n.(Sfloat32)) }
//func (o Sfloat32) Shl1(n Num) Num { return Sfloat32(o << n.(Sfloat32)) } // wrong
//func (o Sfloat32) Shr1(n Num) Num { return Sfloat32(o >> n.(Sfloat32)) } // wrong
//func (o Sfloat64) Add1(n Num) Num { return Sfloat64(o + n.(Sfloat64)) }
//func (o Sfloat64) Sub1(n Num) Num { return Sfloat64(o - n.(Sfloat64)) }
//func (o Sfloat64) Mul1(n Num) Num { return Sfloat64(o * n.(Sfloat64)) }
//func (o Sfloat64) Div1(n Num) Num { return Sfloat64(o / n.(Sfloat64)) }
//func (o Sfloat64) Mod1(n Num) Num { return Sfloat64(o % n.(Sfloat64)) }
//func (o Sfloat64) Shl1(n Num) Num { return Sfloat64(o << n.(Sfloat64)) } // wrong
//func (o Sfloat64) Shr1(n Num) Num { return Sfloat64(o >> n.(Sfloat64)) } // wrong
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
