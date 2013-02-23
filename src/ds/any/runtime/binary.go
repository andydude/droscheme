// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins, Daniel Connelly
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

package ds_any_runtime

import (
.	"ds/any"
	"fmt"
	"strconv"
)

func NewBinary(s string) Binary {
	return Binary([]byte(s))
}

// Our implementation of (seq-first)
func (bv Binary) First() Any {
	return int(bv[0])
}

func (_ Binary) GetType() int {
	return TypeCodeBinary
}

func (bv Binary) GetHash() uintptr {
	return _objectZKhash([]Any(bv.ToVector())...).(uintptr)
}

// Our implementation of (->go-string)
func (bv Binary) GoString() string {
	rv := []byte{}
	for _, value := range bv {
		rv = append(rv, ", "...)
		rv = strconv.AppendInt(rv, int64(value), 10)
	}
	return fmt.Sprintf("Binary([]Any{%s})", rv[2:])
}

// Our implementation of (seq-length)
func (bv Binary) Length() int {
	return len(bv)
}

// Our implementation of (->immutable-string)
func (bv Binary) RawString() string {
	return string([]byte(bv))
}

// Our implementation of (map-ref)
func (bv Binary) Ref(k Any) Any {
	return int(bv[k.(int)])
}

// Our implementation of (seq-rest)
func (bv Binary) Rest() Seq {
	return Binary(bv[1:])
}

// Our implementation of (map-set!)
func (bc Binary) Set(k, v Any) Any {
	bc[k.(int)] = byte(v.(int))
	return _void()
}

// Our implementation of (->scheme-string)
func (bv Binary) SchemeString() string {
	// TODO: test if *revision* is set
	// and if so, then write vu8 instead.
	word := "u8"
	if len(bv) == 0 {
		return "#" + word + "()"
	}
	rv := []byte{}
	for _, value := range bv {
		rv = append(rv, ' ')
		rv = strconv.AppendInt(rv, int64(value), 10)
	}
	return fmt.Sprintf("#%s(%s)", word, rv[1:])
}

func (bv Binary) ToString() String {
	return NewString(bv.RawString())
}

func (bv Binary) ToSymbol() Symbol {
	return NewSymbol(bv.RawString())
}

// Out implementation of (bytevector->u8-vector)
func (bv Binary) ToVector() Vector {
	return _bytevectorZKZRu8ZKvector(bv).(Vector)
}
