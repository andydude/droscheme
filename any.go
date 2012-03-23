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
	TypeCodeSyntax

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
