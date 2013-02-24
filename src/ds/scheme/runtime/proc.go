// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPL): <http://www.gnu.org/licenses/>.

package ds_any_runtime

import (
.	"ds/any"
//	"reflect"
)

/*
 * The Go specification states that map types can have
 * any key types for which == and != are defined, which
 * includes any type except for function, map, or slice.
 * Droscheme uses ALOT of slice types, and so about 4 of the
 * types that implement Any are slice types, so instead of
 * map[Any]Any we have a map from uintptr (the hash type)
 * to a bucket slice, which we iterate through for equality.
 */
type STable struct {
	it      map[uintptr][]Any
	hashFn  Any
	equivFn Any
}

//type PrimSyntax struct {
//	form func(Any, Any, *Env) Any
//	name string
//}
//
//type ProcSyntax struct {
//	form *Proc
//	name string
//}

type CaseSyntax struct {
	env *Env
	expr Any
	lits Any
	body Any
	name string
}

type RulesSyntax struct {
	env *Env
	lits Any
	body Any
	name string
}

type CaseLambdaProc struct {
	name string
	env  *Env
	exprs []Any
}

type SType struct {
	typeName       string
	typeCode       int
	portTypeCode   int
	numberTypeCode int
}

func IsType(o Any, tag int) bool {
	return o.(AnyKinder).GetType() == tag
}

//func Equal(x, y Any) bool {
//	return reflect.DeepEqual(x, y)
//}
//
//func Hash(o Any) uintptr {
//	//return reflect.ValueOf(&o).Pointer()
//	return o.(Hasher).GetHash()
//}

////type Evaler interface {
////	// (object).Eval(environment)
////	Eval(*Env) Any
////}
/// a.k.a. Proc
///type Applier interface {
///	// (procedure).Apply(arguments)
///	Apply(Any) Any
///}
/// a.k.a. Syntax
///type Transformer interface {
///	// (syntax).Transform(keyword, expression, environment)
///	Transform(Any, Any, *Env) Any
///}

// Environments

// Procedures


