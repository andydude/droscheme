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
	"fmt"
)

func NewString(s string) String {
	return String([]rune(s))
}

func (st String) ToBytes() []byte {
	return []byte(string([]rune(st)))
}

func (st String) GoString() string {
	return string([]rune(st))
}

func (_ String) GetType() int {
	return TypeCodeString
}

func (st String) GetHash() uintptr {
	return _objectZKhash([]Any(st.ToVector())...).(uintptr)
}

func (st String) String() string {
	return fmt.Sprintf("\"%s\"", string([]rune(st)))
}

func (st String) Ref(k Any) Any {
	return Char(st[k.(int)])
}

func (st String) Set(k, v Any) Any {
	st[k.(int)] = rune(v.(Char))
	return _void()
}

func (st String) RawString() string {
	return string([]rune(st))
}

func (st String) ToSymbol() Symbol {
	return NewSymbol(st.RawString())
}

func (st String) ToVector() Vector {
	rv := make([]Any, len(st))
	for k, ch := range st {
		rv[k] = Any(ch)
	}
	return Vector(rv)
}
