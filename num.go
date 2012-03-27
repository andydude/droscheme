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
	"fmt"
	"math"
	"math/big"
	"math/cmplx"
	"reflect"
	"strings"
)

const (
	// machine fixnums
	NumberTypeCodeS8 = iota
	NumberTypeCodeS16
	NumberTypeCodeS32
	NumberTypeCodeS64

	// machine flonums
	NumberTypeCodeExactF32
	NumberTypeCodeExactF64

	// abstract numbers
	NumberTypeCodeInteger
	NumberTypeCodeRational
	NumberTypeCodeBaseMax

	// derived numbers bit field
	NumberTypeCodeBaseMask     = 0x7
	NumberTypeCodeUnsigned     = 0x10
	NumberTypeCodeComplex      = 0x20
	NumberTypeCodeComplexPolar = 0x30
	NumberTypeCodeInexact      = 0x40
	NumberTypeCodeReserved     = 0x80
	NumberTypeCodeDerivedMask  = 0x70

	NumberTypeCodeU8         = NumberTypeCodeUnsigned | NumberTypeCodeS8
	NumberTypeCodeU16        = NumberTypeCodeUnsigned | NumberTypeCodeS16
	NumberTypeCodeU32        = NumberTypeCodeUnsigned | NumberTypeCodeS32
	NumberTypeCodeU64        = NumberTypeCodeUnsigned | NumberTypeCodeS64
	NumberTypeCodeNatural    = NumberTypeCodeUnsigned | NumberTypeCodeInteger
	NumberTypeCodeExactC64   = NumberTypeCodeComplex | NumberTypeCodeExactF32
	NumberTypeCodeExactC128  = NumberTypeCodeComplex | NumberTypeCodeExactF64
	NumberTypeCodeInexactS8  = NumberTypeCodeInexact | NumberTypeCodeS8
	NumberTypeCodeInexactS16 = NumberTypeCodeInexact | NumberTypeCodeS16
	NumberTypeCodeInexactS32 = NumberTypeCodeInexact | NumberTypeCodeS32
	NumberTypeCodeInexactS64 = NumberTypeCodeInexact | NumberTypeCodeS64
	NumberTypeCodeInexactU8  = NumberTypeCodeInexact | NumberTypeCodeU8
	NumberTypeCodeInexactU16 = NumberTypeCodeInexact | NumberTypeCodeU16
	NumberTypeCodeInexactU32 = NumberTypeCodeInexact | NumberTypeCodeU32
	NumberTypeCodeInexactU64 = NumberTypeCodeInexact | NumberTypeCodeU64
	NumberTypeCodeF32        = NumberTypeCodeInexact | NumberTypeCodeExactF32
	NumberTypeCodeF64        = NumberTypeCodeInexact | NumberTypeCodeExactF64
	NumberTypeCodeC64        = NumberTypeCodeInexact | NumberTypeCodeExactC64
	NumberTypeCodeC128       = NumberTypeCodeInexact | NumberTypeCodeExactC128

	NumberTypeCodeMax = 0x100
)

type BaseNum interface {
	Any
	GetNumberType() int
}

type Num interface {
	BaseNum
	Add(Num) Num
	Sub(Num) Num
	Mul(Num) Num
	Div(Num) Num
}

type TrigNum interface {
	Num
	ArcCos(Num) Num
	ArcSin(Num) Num
	ArcTan(Num, Num) Num
	Cos(Num) Num
	Sin(Num) Num
	Tan(Num) Num
	Pow(Num) Num
	Log(Num) Num
	Exp() Num
	Ln() Num
}

type IntNum interface {
	Num
	ModEUC(Num) Num
	//ModRTZ(Num) Num
	//ModRTN(Num) Num
	//DivModEUC(IntNum) (IntNum, IntNum)
	//DivModRTZ(IntNum) (IntNum, IntNum)
	//DivModRTP(IntNum) (IntNum, IntNum)
	//DivModRTN(IntNum) (IntNum, IntNum)
	//DivModRTE(IntNum) (IntNum, IntNum)
}

type RealNum interface {
	Num
	Cmp(Num) int // -1, 0, 1
	MakeRect(RealNum) ComplexNum
	MakePolar(RealNum) ComplexNum
	//RoundRTZ() IntNum // truncate
	//RoundRTP() IntNum // ceiling
	//RoundRTN() IntNum // floor
	//RoundRTE() IntNum
}

type ComplexNum interface {
	Num
	Real() RealNum
	Imag() RealNum
	Scale() RealNum
	Angle() RealNum
}

type ExactNum interface {
	Num
	Inexact() InexactNum
}

type InexactNum interface {
	Num
	Exact() ExactNum
	Prec() int
}

func IsNumber(a Any) bool {
	return IsType(a, TypeCodeNumber)
}

func Compare(x Num, y Num) int {
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return x.(RealNum).Cmp(y)
}

// exact
type Sint8 int8
type Sint16 int16
type Sint32 int32
type Sint64 int64 // fixnum
type Suint8 uint8
type Suint16 uint16
type Suint32 uint32
type Suint64 uint64

type SFixnum struct {
	it   Num
	code int
}

type SInteger struct {
	it *big.Int
}

type SRational struct {
	it *big.Rat
}

type SComplex [2]SRational
type SComplexPolar [2]SRational

// inexact
type Sfloat32 float32
type Sfloat64 float64 // flonum
type Scomplex64 complex64
type Scomplex128 complex128

type SFlonum struct {
	it   Num
	code int
	prec int
}

func ToByte(o Any) byte {
	return byte(ToFixnum(o))
}

func IsByte(o Any) bool {
    num := ToFixnum(o)
	return int64(byte(num)) == num
}

func IsInteger(a Any) bool {
	if !IsNumber(a) {
		return false
	}
	switch a.(Num).GetNumberType() &^ NumberTypeCodeInexact {
	case NumberTypeCodeS8:
		return true
	case NumberTypeCodeS16:
		return true
	case NumberTypeCodeS32:
		return true
	case NumberTypeCodeS64:
		return true
	case NumberTypeCodeInteger:
		return true
	}
	return false
}

func IsRational(a Any) bool {
	if !IsNumber(a) {
		return false
	}
	switch a.(Num).GetNumberType() &^ NumberTypeCodeInexact {
	case NumberTypeCodeRational:
		return true
	}
	return false
}

func IsReal(a Any) bool {
	if !IsNumber(a) {
		return false
	}
	if a.(Num).GetNumberType()&NumberTypeCodeComplex != 0 {
		return false
	}
	return true
}

func IsComplex(a Any) bool {
	if !IsNumber(a) {
		return false
	}
	if a.(Num).GetNumberType()&NumberTypeCodeComplex != 0 {
		return true
	}
	return false
}

func ToFixnum(a Any) int64 {
	switch a.(type) {
	case Sint64:
		return reflect.ValueOf(a).Int()
	case Sfloat64:
		return int64(float64(a.(Sfloat64)))
	}
	// TODO: fix ERROR
	return 0
}

func ToFlonum(a Any) float64 {
	switch a.(type) {
	case Sint64:
		return float64(int64(a.(Sint64)))
	case Sfloat64:
		return reflect.ValueOf(a).Float()
	}
	// TODO: fix ERROR
	return 0.0
}

func ToFcmplx(a Any) complex128 {
	return reflect.ValueOf(a).Complex()
}

func ToInteger(n Num) SInteger {
	switch n.(type) {
	case Sint64:
		return SInteger{it: big.NewInt(int64(n.(Sint64)))}
	case SInteger:
		return n.(SInteger)
	}
	panic("ToRational()")
}

func ToRational(n Num) SRational {
	switch n.(type) {
	case Sint64:
		return SRational{it: big.NewRat(0, 1).SetInt64(int64(n.(Sint64)))}
	//case SFixnum:
	//	return SRational{it: big.NewRat(0, 1).SetInt64(n.(SFixnum).it.(Sint64))}
	case SInteger:
		return SRational{it: big.NewRat(0, 1).SetInt(n.(SInteger).it)}
	case SRational:
		return n.(SRational)
	}
	panic("ToRational()")
}

func NewInteger64(n int64) SInteger {
	return SInteger{it: big.NewInt(n)}
}

func NewInteger(n Num) Num {
	return SInteger{it: big.NewInt(int64(n.(Sint64)))}
}

func NewRational64(n, d int64) SRational {
	return SRational{it: big.NewRat(n, d)}
}

func NewRational(n, d Num) SRational {
	return SRational{it: big.NewRat(int64(n.(Sint64)), int64(d.(Sint64)))}
}

func NewComplexI() SComplex {
	return SComplex{NewRational64(0, 1), NewRational64(1, 1)}
}

func NewComplex(x, y Num) ComplexNum {
	t := unifyType(x.GetNumberType(), y.GetNumberType())
	if isComplexType(t) {
		panic(newTypeError("expected real number"))
	}
	if isInexactType(t) {
		return Scomplex128(complex(ToFlonum(x), ToFlonum(y)))
	}
	return SComplex{ToRational(x), ToRational(y)}
}

func NewComplexPolar(s, a Num) ComplexNum {
	t := unifyType(s.GetNumberType(), a.GetNumberType())
	if isComplexType(t) {
		panic(newTypeError("expected real number"))
	}
	if isInexactType(t) {
		scale := ToFlonum(s)
		angle := ToFlonum(a)
		x := scale * math.Cos(angle)
		y := scale * math.Sin(angle)
		return Scomplex128(complex(x, y))
	}
	return SComplexPolar{ToRational(s), ToRational(a)}
}

func isComplexType(t int) bool {
	return t&NumberTypeCodeComplex != 0
}

func isInexactType(t int) bool {
	return t&NumberTypeCodeInexact != 0
}

func isUnsignedType(t int) bool {
	return t&NumberTypeCodeUnsigned != 0
}

func isMachineIntegerType(t int) bool {
	if isComplexType(t) {
		return false
	}
	if isInexactType(t) {
		return false
	}
	switch t & NumberTypeCodeBaseMask {
	case NumberTypeCodeS8:
		return true
	case NumberTypeCodeS16:
		return true
	case NumberTypeCodeS32:
		return true
	case NumberTypeCodeS64:
		return true
	}
	return false
}

func isMachineRealType(t int) bool {
	if isComplexType(t) {
		return false
	}
	if !isInexactType(t) {
		return false
	}
	if isUnsignedType(t) {
		return false
	}
	switch t & NumberTypeCodeBaseMask {
	case NumberTypeCodeExactF32:
		return true
	case NumberTypeCodeExactF64:
		return true
	}
	return false
}

func isMachineType(t int) bool {
	if isMachineRealType(t) {
		return true
	}
	if isMachineIntegerType(t) {
		return true
	}
	if !isComplexType(t) {
		return false
	}
	switch t & NumberTypeCodeBaseMask {
	case NumberTypeCodeExactF32:
		return true
	case NumberTypeCodeExactF64:
		return true
	}
	return false
}

/* unifyComplexType()
 *
 * The 4 complex number types are as follows:
 *   - exact real (Sint64, SInteger, SRational)
 *   - exact complex (SComplex, SComplexPolar)
 *   - inexact real (Sfloat64, SInteger)
 *   - inexact complex (Scomplex128)
 *
 * This function returns one of:
 *    (code)                (t >> 4)
 *   - 0x00 real                  0
 *   - 0x10 unsigned              0
 *   - 0x20 complex               2
 *   - 0x30 complex-polar         2
 *   - 0x40 inexact real          4
 *   - 0x50 inexact unsigned      4
 *   - 0x60 inexact complex       6
 *   - 0x70 inexact complex-polar 6
 */
func unifyComplexType(r, s int) (t int) {
	return (r | s) &^ NumberTypeCodeBaseMask &^ NumberTypeCodeUnsigned
}

func unifyType(r, s int) (t int) {
	switch unifyComplexType(r, s) {

	case 0: // exact real
		if isMachineIntegerType(r) &&
			isMachineIntegerType(s) {
			return NumberTypeCodeS64
		} else {
			return NumberTypeCodeRational
		}

	case NumberTypeCodeComplex: // exact complex
		if r == NumberTypeCodeComplexPolar &&
			s == NumberTypeCodeComplexPolar {
			return NumberTypeCodeComplexPolar
		} else {
			return NumberTypeCodeComplex
		}

	case NumberTypeCodeInexact: // inexact real
		return NumberTypeCodeF64

	default: // inexact complex
		return NumberTypeCodeC128
	}

	panic("unreachable")
	return
}

func unify2(args Any) (x, y Num) {
	a, b := unlist2(args)
	x = a.(Num)
	y = b.(Num)
	if x.GetNumberType() != y.GetNumberType() {
		x, y = unify(x, y)
	}
	return
}

func unify(a, b Num) (x, y Num) {
	switch unifyType(a.GetNumberType(), b.GetNumberType()) {
	case NumberTypeCodeS64:
		x = Sint64(ToFixnum(a))
		y = Sint64(ToFixnum(b))
	case NumberTypeCodeF64:
		x = Sfloat64(ToFlonum(a))
		y = Sfloat64(ToFlonum(b))
	case NumberTypeCodeC128:
		x = Scomplex128(ToFcmplx(a))
		y = Scomplex128(ToFcmplx(b))
	case NumberTypeCodeInteger:
		x = ToInteger(a)
		y = ToInteger(b)
	case NumberTypeCodeRational:
		x = ToRational(a) //.(SRational).Nmtr(), a.(SRational).Dmtr())
		y = ToRational(b) //.(SRational).Nmtr(), b.(SRational).Dmtr())
	case NumberTypeCodeComplex:
		//x = NewComplex(a)
		//y = NewComplex(b)
	case NumberTypeCodeComplexPolar:
		//x = NewComplexPolar(a)
		//y = NewComplexPolar(b)
	}
	return
}

// base numbers
func (o Sint8) GetType() int             { return TypeCodeNumber }
func (o Sint8) GetNumberType() int       { return NumberTypeCodeS8 }
func (o Sint16) GetType() int            { return TypeCodeNumber }
func (o Sint16) GetNumberType() int      { return NumberTypeCodeS16 }
func (o Sint32) GetType() int            { return TypeCodeNumber }
func (o Sint32) GetNumberType() int      { return NumberTypeCodeS32 }
func (o Sint64) GetType() int            { return TypeCodeNumber }
func (o Sint64) GetNumberType() int      { return NumberTypeCodeS64 }
func (o Suint8) GetType() int            { return TypeCodeNumber }
func (o Suint8) GetNumberType() int      { return NumberTypeCodeU8 }
func (o Suint16) GetType() int           { return TypeCodeNumber }
func (o Suint16) GetNumberType() int     { return NumberTypeCodeU16 }
func (o Suint32) GetType() int           { return TypeCodeNumber }
func (o Suint32) GetNumberType() int     { return NumberTypeCodeU32 }
func (o Suint64) GetType() int           { return TypeCodeNumber }
func (o Suint64) GetNumberType() int     { return NumberTypeCodeU64 }
func (o Sfloat32) GetType() int          { return TypeCodeNumber }
func (o Sfloat32) GetNumberType() int    { return NumberTypeCodeF32 }
func (o Sfloat64) GetType() int          { return TypeCodeNumber }
func (o Sfloat64) GetNumberType() int    { return NumberTypeCodeF64 }
func (o Scomplex64) GetType() int        { return TypeCodeNumber }
func (o Scomplex64) GetNumberType() int  { return NumberTypeCodeC64 }
func (o Scomplex128) GetType() int       { return TypeCodeNumber }
func (o Scomplex128) GetNumberType() int { return NumberTypeCodeC128 }

// derived numbers
func (o SFixnum) GetType() int             { return TypeCodeNumber }
func (o SFixnum) GetNumberType() int       { return o.code }
func (o SFlonum) GetType() int             { return TypeCodeNumber }
func (o SFlonum) GetNumberType() int       { return o.code }
func (o SInteger) GetType() int            { return TypeCodeNumber }
func (o SInteger) GetNumberType() int      { return NumberTypeCodeInteger }
func (o SRational) GetType() int           { return TypeCodeNumber }
func (o SRational) GetNumberType() int     { return NumberTypeCodeRational }
func (o SComplex) GetType() int            { return TypeCodeNumber }
func (o SComplex) GetNumberType() int      { return NumberTypeCodeComplex }
func (o SComplexPolar) GetType() int       { return TypeCodeNumber }
func (o SComplexPolar) GetNumberType() int { return NumberTypeCodeComplexPolar }

// S32

func (o Sint32) String() string {
	return fmt.Sprintf("%d", o)
}
func (o Sint32) Equal(n Any) bool { return o == n.(Sint32) }
func (o Sint32) Cmp(n Num) int {
	if o < n.(Sint32) {
		return -1
	}
	if o > n.(Sint32) {
		return 1
	}
	return 0
}
func (o Sint32) Add(n Num) Num { return Sint32(o + n.(Sint32)) }
func (o Sint32) Sub(n Num) Num { return Sint32(o - n.(Sint32)) }
func (o Sint32) Mul(n Num) Num { return Sint32(o * n.(Sint32)) }
func (o Sint32) Div(n Num) Num { return Sint32(o / n.(Sint32)) }
func (o Sint32) Mod(n Num) Num { return Sint32(o % n.(Sint32)) }
func (o Sint32) Shl(n Num) Num { return Sint32(o << uint(n.(Sint32))) }
func (o Sint32) Shr(n Num) Num { return Sint32(o >> uint(n.(Sint32))) }

// S64

func (o Sint64) MakeRect(n RealNum) ComplexNum {
	var x, y int64 = int64(o), int64(n.(Sint64))
	return NewComplex(NewRational64(x, 0), NewRational64(y, 0))
}
func (o Sint64) MakePolar(n RealNum) ComplexNum {
	var x, y int64 = int64(o), int64(n.(Sint64))
	return NewComplexPolar(NewRational64(x, 0), NewRational64(y, 0))
}
func (o Sint64) String() string {
	return fmt.Sprintf("%d", o)
}
func (o Sint64) Equal(n Any) bool { return o == n.(Sint64) }
func (o Sint64) Cmp(n Num) int {
	if o < n.(Sint64) {
		return -1
	}
	if o > n.(Sint64) {
		return 1
	}
	return 0
}
func (o Sint64) Add(n Num) Num { return Sint64(o + n.(Sint64)) }
func (o Sint64) Sub(n Num) Num { return Sint64(o - n.(Sint64)) }
func (o Sint64) Mul(n Num) Num { return Sint64(o * n.(Sint64)) }
func (o Sint64) Div(n Num) Num { return Sint64(o / n.(Sint64)) }
func (o Sint64) Mod(n Num) Num { return Sint64(o % n.(Sint64)) }
func (o Sint64) Shl(n Num) Num { return Sint64(o << uint(n.(Sint64))) }
func (o Sint64) Shr(n Num) Num { return Sint64(o >> uint(n.(Sint64))) }

//// U32
//func (o Suint32) Add(n Num) Num { return Suint32(o + n.(Suint32)) }
//func (o Suint32) Sub(n Num) Num { return Suint32(o - n.(Suint32)) }
//func (o Suint32) Mul(n Num) Num { return Suint32(o * n.(Suint32)) }
//func (o Suint32) Div(n Num) Num { return Suint32(o / n.(Suint32)) }
//func (o Suint32) Mod(n Num) Num { return Suint32(o % n.(Suint32)) }
//func (o Suint32) Shl(n Num) Num { return Suint32(o << n.(Suint32)) }
//func (o Suint32) Shr(n Num) Num { return Suint32(o >> n.(Suint32)) }
//
//// U64
//func (o Suint64) Add(n Num) Num { return Suint64(o + n.(Suint64)) }
//func (o Suint64) Sub(n Num) Num { return Suint64(o - n.(Suint64)) }
//func (o Suint64) Mul(n Num) Num { return Suint64(o * n.(Suint64)) }
//func (o Suint64) Div(n Num) Num { return Suint64(o / n.(Suint64)) }
//func (o Suint64) Mod(n Num) Num { return Suint64(o % n.(Suint64)) }
//func (o Suint64) Shl(n Num) Num { return Suint64(o << n.(Suint64)) }
//func (o Suint64) Shr(n Num) Num { return Suint64(o >> n.(Suint64)) }

// F32

func (o Sfloat32) String() string {
	return strings.Trim(fmt.Sprintf("%f", o), "0") + "s0"
}
func (o Sfloat32) Equal(n Any) bool { return o == n.(Sfloat32) }
func (o Sfloat32) Cmp(n Num) int {
	if o < n.(Sfloat32) {
		return -1
	}
	if o > n.(Sfloat32) {
		return 1
	}
	return 0
}
func (o Sfloat32) Add(n Num) Num { return Sfloat32(o + n.(Sfloat32)) }
func (o Sfloat32) Sub(n Num) Num { return Sfloat32(o - n.(Sfloat32)) }
func (o Sfloat32) Mul(n Num) Num { return Sfloat32(o * n.(Sfloat32)) }
func (o Sfloat32) Div(n Num) Num { return Sfloat32(o / n.(Sfloat32)) }
func (o Sfloat32) Mod(n Num) Num { return Sfloat32(0) } // wrong
func (o Sfloat32) Shl(n Num) Num { return Sfloat32(0) } // wrong
func (o Sfloat32) Shr(n Num) Num { return Sfloat32(0) } // wrong

// F64

func (o Sfloat64) MakeRect(n RealNum) ComplexNum {
	return NewComplex(o, Sfloat64(ToFlonum(n)))
}
func (o Sfloat64) MakePolar(n RealNum) ComplexNum {
	return NewComplexPolar(o, Sfloat64(ToFlonum(n)))
}
func (o Sfloat64) String() string {
	return strings.Trim(fmt.Sprintf("%f", o), "0")
}
func (o Sfloat64) Equal(n Any) bool { return o == n.(Sfloat64) }
func (o Sfloat64) Cmp(n Num) int {
	if o < n.(Sfloat64) {
		return -1
	}
	if o > n.(Sfloat64) {
		return 1
	}
	return 0
}
func (o Sfloat64) Add(n Num) Num { return Sfloat64(o + n.(Sfloat64)) }
func (o Sfloat64) Sub(n Num) Num { return Sfloat64(o - n.(Sfloat64)) }
func (o Sfloat64) Mul(n Num) Num { return Sfloat64(o * n.(Sfloat64)) }
func (o Sfloat64) Div(n Num) Num { return Sfloat64(o / n.(Sfloat64)) }
func (o Sfloat64) Mod(n Num) Num { return Sfloat64(0) } // wrong
func (o Sfloat64) Shl(n Num) Num { return Sfloat64(0) } // wrong
func (o Sfloat64) Shr(n Num) Num { return Sfloat64(0) } // wrong

//func (o Scomplex64) Add(n Num) Num { return Scomplex64(o + n.(Scomplex64)) }
//func (o Scomplex64) Sub(n Num) Num { return Scomplex64(o - n.(Scomplex64)) }
//func (o Scomplex64) Mul(n Num) Num { return Scomplex64(o * n.(Scomplex64)) }
//func (o Scomplex64) Div(n Num) Num { return Scomplex64(o / n.(Scomplex64)) }
//func (o Scomplex64) Mod(n Num) Num { return Scomplex64(o % n.(Scomplex64)) }
//func (o Scomplex64) Shl(n Num) Num { return Scomplex64(o << n.(Scomplex64)) } // wrong
//func (o Scomplex64) Shr(n Num) Num { return Scomplex64(o >> n.(Scomplex64)) } // wrong
//func (o Scomplex128) Add(n Num) Num { return Scomplex128(o + n.(Scomplex128)) }
//func (o Scomplex128) Sub(n Num) Num { return Scomplex128(o - n.(Scomplex128)) }
//func (o Scomplex128) Mul(n Num) Num { return Scomplex128(o * n.(Scomplex128)) }
//func (o Scomplex128) Div(n Num) Num { return Scomplex128(o / n.(Scomplex128)) }
//func (o Scomplex128) Mod(n Num) Num { return Scomplex128(o % n.(Scomplex128)) }
//func (o Scomplex128) Shl(n Num) Num { return Scomplex128(o << n.(Scomplex128)) } // wrong
//func (o Scomplex128) Shr(n Num) Num { return Scomplex128(o >> n.(Scomplex128)) } // wrong

// Integer

func (o SInteger) MakeRect(n RealNum) ComplexNum {
	return NewComplex(o, Sfloat64(ToFlonum(n)))
}
func (o SInteger) MakePolar(n RealNum) ComplexNum {
	return NewComplexPolar(o, Sfloat64(ToFlonum(n)))
}
func (o SInteger) String() string {
	return o.it.String()
}
func (o SInteger) Equal(n Any) bool {
	return o.Cmp(n.(SInteger)) == 0
}
func (o SInteger) Cmp(n Num) int {
	return o.it.Cmp(n.(SInteger).it)
}
func (o SInteger) Add(n Num) Num {
	return SInteger{it: big.NewInt(0).Add(o.it, n.(SInteger).it)}
}
func (o SInteger) Sub(n Num) Num {
	return SInteger{it: big.NewInt(0).Sub(o.it, n.(SInteger).it)}
}
func (o SInteger) Mul(n Num) Num {
	return SInteger{it: big.NewInt(0).Mul(o.it, n.(SInteger).it)}
}
func (o SInteger) Div(n Num) Num {
	return SInteger{it: big.NewInt(0).Div(o.it, n.(SInteger).it)}
}
func (o SInteger) Mod(n Num) Num {
	return SInteger{it: big.NewInt(0).Mod(o.it, n.(SInteger).it)}
}
func (o SInteger) Shl(n Num) Num {
	return SInteger{it: big.NewInt(0).Rsh(o.it, uint(ToFixnum(n)))}
}
func (o SInteger) Shr(n Num) Num {
	return SInteger{it: big.NewInt(0).Lsh(o.it, uint(ToFixnum(n)))}
}

// Rational

func (o SRational) MakeRect(n RealNum) ComplexNum {
	return NewComplex(o, n.(SRational))
}
func (o SRational) MakePolar(n RealNum) ComplexNum {
	return NewComplexPolar(o, n.(SRational))
}
func (o SRational) String() string {
	return o.it.RatString()
}
func (o SRational) Equal(n Any) bool {
	return o.Cmp(n.(SRational)) == 0
}
func (o SRational) Zero() Num {
	return SRational{it: big.NewRat(0, 1)}
}
func (o SRational) Cmp0() int {
	return o.it.Cmp(big.NewRat(0, 1))
}
func (o SRational) Cmp(n Num) int {
	return o.it.Cmp(n.(SRational).it)
}
func (o SRational) Add(n Num) Num {
	return SRational{it: big.NewRat(0, 1).Add(o.it, n.(SRational).it)}
}
func (o SRational) Sub(n Num) Num {
	return SRational{it: big.NewRat(0, 1).Sub(o.it, n.(SRational).it)}
}
func (o SRational) Mul(n Num) Num {
	return SRational{it: big.NewRat(0, 1).Mul(o.it, n.(SRational).it)}
}
func (o SRational) Div(n Num) Num { return Sfloat64(0) } // wrong
func (o SRational) Mod(n Num) Num { return Sfloat64(0) } // wrong
func (o SRational) Shl(n Num) Num { return Sfloat64(0) } // wrong
func (o SRational) Shr(n Num) Num { return Sfloat64(0) } // wrong

func (o SRational) Dmtr() Num {
	return SInteger{it: o.it.Denom()}
}

func (o SRational) Nmtr() Num {
	return SInteger{it: o.it.Num()}
}

// C128

func (o Scomplex128) String() string {
	return fmt.Sprintf("%v+%vi", o.Real(), o.Imag())
}
func (o Scomplex128) Equal(n Any) bool {
	return o == n.(Scomplex128)
}
func (o Scomplex128) Real() RealNum {
	return Sfloat64(real(o))
}
func (o Scomplex128) Imag() RealNum {
	return Sfloat64(imag(o))
}
func (o Scomplex128) Scale() RealNum {
	return Sfloat64(cmplx.Abs(complex128(o)))
}
func (o Scomplex128) Angle() RealNum {
	return Sfloat64(cmplx.Phase(complex128(o)))
}
func (o Scomplex128) Add(n Num) Num { return Scomplex128(o + n.(Scomplex128)) }
func (o Scomplex128) Sub(n Num) Num { return Scomplex128(o - n.(Scomplex128)) }
func (o Scomplex128) Mul(n Num) Num { return Scomplex128(o * n.(Scomplex128)) }
func (o Scomplex128) Div(n Num) Num { return Scomplex128(o / n.(Scomplex128)) }
func (o Scomplex128) Mod(n Num) Num { return Scomplex128(0) } // wrong
func (o Scomplex128) Shl(n Num) Num { return Scomplex128(0) } // wrong
func (o Scomplex128) Shr(n Num) Num { return Scomplex128(0) } // wrong

// Complex

func (o SComplex) String() string {
	if o[1].Cmp0() == -1 {
		return fmt.Sprintf("%s-%si", o[0], big.NewRat(0, 1).Neg(o[1].it).RatString())
	}
	return fmt.Sprintf("%s+%si", o[0], o[1])
}
func (o SComplex) Conj() Num {
	return SComplex{o[0], SRational{it: big.NewRat(0, 1).Neg(o[1].it)}}
}
func (o SComplex) Real() RealNum {
	return o[0]
}
func (o SComplex) Imag() RealNum {
	return o[1]
}
func (o SComplex) Scale() RealNum {
	return o[1] //wrong
}
func (o SComplex) Angle() RealNum {
	return o[1] //wrong
}
func (o SComplex) Equal(n Any) bool { return Equal(o, n) }
func (o SComplex) Add(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplex) Sub(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplex) Mul(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplex) Div(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplex) Mod(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplex) Shl(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplex) Shr(n Num) Num    { return Sfloat64(0) } // wrong

// ComplexPolar

func (o SComplexPolar) String() string {
	return fmt.Sprintf("%s@%s", o[0], o[1])
}
func (o SComplexPolar) Real() RealNum {
	return o[1] //wrong
}
func (o SComplexPolar) Imag() RealNum {
	return o[1] //wrong
}
func (o SComplexPolar) Scale() RealNum {
	return o[0]
}
func (o SComplexPolar) Angle() RealNum {
	return o[1]
}
func (o SComplexPolar) Equal(n Any) bool { return Equal(o, n) }
func (o SComplexPolar) Cmp(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplexPolar) Add(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplexPolar) Sub(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplexPolar) Mul(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplexPolar) Div(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplexPolar) Mod(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplexPolar) Shl(n Num) Num    { return Sfloat64(0) } // wrong
func (o SComplexPolar) Shr(n Num) Num    { return Sfloat64(0) } // wrong
