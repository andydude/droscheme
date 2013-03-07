// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

// Interfaces
//
// These types are used for anything that supports them.
// 'any' is a synonym for Go interface{} objects, which 
// represents any object. Previously, the Any interface
// had Equaler and Hasher methods on it, which required
// that only user-defined types could become members,
// as time passed, it became clear that types such as
// bool or char or int32 or float64 should also be able
// to have Equaler and Hasher methods for them, and
// after realizing that this was impossible in the Go
// runtime, the requirement that every object implement
// Equaler and Hasher methods was relaxed.
package ds_any

type (
	Applier interface {
		Apply(args []interface{}) interface{}
	}

	Evaler interface {
		Eval(env *Env) interface{}
	}

	Conser interface {
		Cons(obj interface{}) Conser
		Car() interface{}
		Cdr() Conser
	}

	Equaler interface {
		Equal(obj interface{}) bool
	}

	Hasher interface {
		Hash() uintptr
	}

	Kinder interface {
		Kind() int
	}

	Matcher interface {
		Match(syntax interface{}, env *Env) bool
	}

	Replacer interface {
		Replace(env *Env) interface{}
	}

	Transformer interface {
		Transform(syntax interface{}, env *Env) interface{}
	}

	SchemeStringer interface {
		SchemeString() string
	}
)
