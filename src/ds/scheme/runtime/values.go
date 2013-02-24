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

func (_ Values) GetType() int {
	return TypeCodeValues
}

func (_ Void) GetHash() uintptr {
	return 0
}

func (_ Void) GetType() int {
	return TypeCodeVoid
}

func (_ Void) Equal(a Any) bool {
	return _voidZS(a).(bool)
}

func (_ Void) String() string {
	return ""
}

var emptyZS = NewProc(_emptyZS, "empty?")
func _emptyZS(a Any) Any {
    switch a.(type) {
    case *Null:
        return true
    case *Void:
        return true
    case Binary:
		return len(a.(Binary)) == 0
    case String:
        return len(a.(String)) == 0
    case Symbol:
		return len(a.(Symbol).name) == 0
    case Vector:
        return len(a.(Vector)) == 0
    case Values:
		return len(a.(Values)) == 0
    }
    return false
}

var values = NewProc(_values, "values")
func _values(o ...Any) Any {
	return Values(o)
}
