//
// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins, Daniel Connelly
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
//
package droscheme

import (
	"reflect"
)

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

/* Any
 *
 * This interface abstracts all data.
 *
 * The first method is used by everything.
 * The second and third method are used by
 * hashtables mostly, but also by equal?,
 * eqv?, eq?, and other equality functions.
 */
type Any interface {
//	GetType() int
//	GetHash() uintptr
//	Equal(Any) bool
}

type (
	AnyKinder interface {
		GetType() int
	}
	Kinder interface {
		Kind() reflect.Kind
	}
	Comparer interface {
		Compare(Any) int
	}
	Equaler interface {
		Equal(Any) bool
	}
	Hasher interface {
		GetHash() uintptr
	}
	Seq interface {
		Length() int
	}
)

/* Matcher
 * 
 * This interface represents the first phase
 * of (syntax-rules) transformations. It is
 * similar to Equal, except that it takes an
 * environment, which is normally empty, and
 * is used as an extra return value in which
 * pattern variables are bound.
 *
 * The pattern variables are used by the next
 * phase, so in order to save energy later, we
 * assign any pattern variables found in the
 * object that implements this method and then
 * store them in env. The same environment is
 * also used for literals, which must be given
 * to the Match method in the form x=x.
 */
type Matcher interface {
	Match(syntax Any, env *Env) bool
}

/* Replacer
 *
 * This interface represents the second phase
 * of (syntax-rules) transformations. It is
 * similar to Eval, except in the way that the
 * environment is used. Instead of raising an
 * error every time a symbol is unbound, it
 * continues silently, and only replaces those
 * symbols which are in the environment.
 *
 * The pattern variables are given in env by the
 * Match method, in such a way that we can treat
 * literals the same as pattern variables, since
 * they are of the form x=x (see above). Hence,
 * the literals can safely be replaced just like
 * the pattern variables. This is the magic
 * I was hoping to describe in this comment.
 */
type Replacer interface {
	Replace(env *Env) Any
}

func IsType(o Any, tag int) bool {
	return o.(AnyKinder).GetType() == tag
}

func Equal(x, y Any) bool {
	return reflect.DeepEqual(x, y)
}

func Hash(o Any) uintptr {
	//return reflect.ValueOf(&o).Pointer()
	return o.(Hasher).GetHash()
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

