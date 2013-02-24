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
	"reflect"
)

type (
	Char rune
	Void struct{}
	Null struct{}
	Pair struct {
		car Any
		cdr Any
	}
	Binary []byte
	String []rune
	Vector []Any
	Values []Any
	Symbol struct {
		name string
	}
)

type (
	Error interface {
		Error() string
		Irritants() Any
	}
	GoStringer interface {
		GoString() string
	}
	SchemeStringer interface {
		SchemeString() string
	}
)

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
	Map interface {
		Define(key, value Any)
		Ref(key Any) Any
		RefDefault(key, value Any) Any
		Set(key, value Any)
	}
	AdvancedMap interface {
		Map
		Add(value Named)
	}
	Seq interface {
		First() Any
		Length() int
		Rest() Seq
	}
)

