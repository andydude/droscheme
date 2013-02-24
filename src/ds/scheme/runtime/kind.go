// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

package ds_any_runtime

import (
.	"ds/any"
)

// TODO: s/TypeCode/Kind/g
const (
	TypeCodeAny     = iota
	TypeCodeType    // go:Type
	TypeCodeNull    // go:Null      s:null?       -- Evaler interface
	TypeCodePair    // go:Pair      s:pair?       -- Evaler interface
	TypeCodeChar    // go:Char      s:char?
	TypeCodeBool    // go:Bool      s:boolean?
	TypeCodeProc    // go:Proc      s:procedure?  -- Applier interface
	TypeCodeBinary  // go:Binary    s:bytevector? -- Seq interface
	TypeCodeNumber  // go:Num       s:number?     -- Num interface
	TypeCodePort    // go:Port      s:port?       -- Port interface
	TypeCodeString  // go:String    s:string?     -- Seq interface
	TypeCodeSymbol  // go:Symbol    s:symbol?     -- Evaler interface
	TypeCodeVector  // go:Vector    s:vector?     -- Seq interface
	TypeCodeTable   // go:Table     s:hashtable?
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

func _kindZKof(a Any) int {
	switch a.(type) {
	case int8: return TypeCodeNumber
	case int16: return TypeCodeNumber
	case int32: return TypeCodeNumber
	case int64: return TypeCodeNumber
	case uint8: return TypeCodeNumber
	case uint16: return TypeCodeNumber
	case uint32: return TypeCodeNumber
	case uint64: return TypeCodeNumber
	case float32: return TypeCodeNumber
	case float64: return TypeCodeNumber
	case complex64: return TypeCodeNumber
	case complex128: return TypeCodeNumber
	case bool: return TypeCodeBool
	case Char:	return TypeCodeChar
	case *Void:	return TypeCodeVoid
	case *Null: return TypeCodeNull
	case *Pair: return TypeCodePair
	case *Proc: return TypeCodeProc
	case *LambdaProc: return TypeCodeProc
	//case SType: return TypeCodeType
	case Binary: return TypeCodeBinary
	case String: return TypeCodeString
	case Symbol: return TypeCodeSymbol
	case Values: return TypeCodeValues
	case Vector: return TypeCodeVector
	//case SFrame: return TypeCodeFrame
	case STable: return TypeCodeTable
	case *Env: return TypeCodeEnvSpec
	//case *PrimSyntax: return TypeCodeSyntax
	//case *CaseSyntax: return TypeCodeSyntax
	//case *RuleSyntax: return TypeCodeSyntax
	}
	return a.(AnyKinder).GetType()
}

func _kindZKZRstring(ak Any) string {
	switch ak.(int) {
	case TypeCodeBool:
		return "boolean"
	case TypeCodeChar:
		return "char"
	case TypeCodeNull:
		return "null"
	case TypeCodePair:
		return "pair"
	case TypeCodeProc:
		return "procedure"
	case TypeCodeVoid:
		return "void"
	case TypeCodeBinary:
		return "bytevector"
	case TypeCodeString:
		return "string"
	case TypeCodeSymbol:
		return "symbol"
	case TypeCodeValues:
		return "values"
	case TypeCodeVector:
		return "vector"
	case TypeCodeTable:
		return "hashtable"
	case TypeCodeNumber:
		return "number"
	case TypeCodePort:
		return "port"
	case TypeCodeEnvSpec:
		return "environment"
	case TypeCodeSyntax:
		return "syntax"
	}
	_error("kind->string expected a recognized type")
	return ""
}

