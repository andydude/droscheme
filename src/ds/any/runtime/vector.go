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

// Our implementation of (object-type)
func (vc Vector) GetType() int {
    return TypeCodeVector
}

// Our implementation of (object-hash)
func (vc Vector) GetHash() uintptr {
	return _objectZKhash([]Any(vc)...).(uintptr)
}

// Our implementation of (object-equal?)
func (vc Vector) Equal(a Any) bool {
    return _objectZKequalZS(vc, a).(bool)
}

// Our implementation of (eval)
func (vc Vector) Eval(env *Env) Any {
    var rv = _makeZKvector(len(vc)).(Vector)
    for i := 0; i < len(vc); i++ {
        rv[i] = _eval(vc[i], env)
    }
    return rv
}

// Our implementation of (map-ref)
func (vc Vector) Ref(k Any) Any {
    return vc[k.(int)]
}

// Our implementation of (map-set!)
func (vc Vector) Set(k, v Any) Any {
    vc[k.(int)] = v
    return _void()
}

// Our implementation of (->scheme-string)
func (vc Vector) SchemeString() string {
	if len(vc) == 0 {
		return "#()"
	}
	rv := []byte{}
	for _, value := range vc {
		rv = append(rv, ' ')
		rv = append(rv, _ZKZRschemeZKstring(value).(string)...)
	}
	return fmt.Sprintf("#(%s)", rv[1:])
}

func (vc Vector) ToBinary() Binary {
	rv := make([]byte, len(vc))
	for k, it := range vc {
		rv[k] = it.(byte)
	}
	return Binary(rv)
}

// Our implementation of (vector->list)
func (vc Vector) ToList() Any {
    if len(vc) == 0 {
		return _null()
    }
    return _cons(vc[0], Vector(vc[1:]).ToList())
}

// Our implementation of (vector->string)
func (vc Vector) ToString() String {
	rv := make([]rune, len(vc))
	for k, it := range vc {
		rv[k] = it.(rune)
	}
	return String(rv)
}
