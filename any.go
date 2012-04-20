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
	TypeCodeAny     = iota
	TypeCodeType    // go:SType
	TypeCodeNull    // go:SNull     s:null?       -- Evaler interface
	TypeCodePair    // go:SPair     s:pair?       -- Evaler interface
	TypeCodeChar    // go:SChar     s:char?
	TypeCodeBool    // go:SBool     s:boolean?
	TypeCodeProc    // go:SProc     s:procedure?  -- Applier interface
	TypeCodeBinary  // go:SBinary   s:bytevector? -- Seq interface
	TypeCodeNumber  // go:Num       s:number?     -- Num interface
	TypeCodePort    // go:Port      s:port?       -- Port interface
	TypeCodeString  // go:SString   s:string?     -- Seq interface
	TypeCodeSymbol  // go:SSymbol   s:symbol?     -- Evaler interface
	TypeCodeVector  // go:SVector   s:vector?     -- Seq interface
	TypeCodeTable   // go:STable    s:hashtable?
	TypeCodeRecord  // go:Record                  -- interface
	TypeCodeLibrary //
	TypeCodeValues  // multiple return values
	TypeCodeSyntax  //                            -- Transformer interface
	TypeCodeEnvSpec
	TypeCodeError
	TypeCodeLabel
	TypeCodeVoid

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
// Evaler
// Applier
// Transformer

type Any interface {
	GetType() int
	GetHash() uintptr
	Equal(Any) bool
}

func IsType(o Any, tag int) bool {
	return o.GetType() == tag
}

func Equal(x, y Any) bool {
	return reflect.DeepEqual(x, y)
}

func Hash(o Any) uintptr {
	//return reflect.ValueOf(&o).Pointer()
	return o.GetHash()
}

type Error interface {
	error
	Irritants() Any
}

type Evaler interface {
	// (object).Eval(environment)
	Eval(*Env) Any
}

type Applier interface {
	// (procedure).Apply(arguments)
	Apply(Any) Any
}

type Transformer interface {
	// (syntax).Transform(keyword, expression, environment)
	Transform(Any, Any, *Env) Any
}

// TODO: make a table of STypes with type names etc.
//GetTypeName() string can come form table
//GetTypeInfo() Any can come from table

