// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins, Daniel Connelly
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

package ds_any_syntax
import (
.	"ds/any"
)

// (begin expr ...)
var begin = NewSyntax(_begin, "begin")
func _begin(env *Env, exprs...Any) Any {
    var value = _void()
	for _, expr := range exprs {
		value = _eval(expr, env)
    }
    return value
}

// (if test consequent)
// (if test consequent alternate)
// (if test c1 t2 c2 t3 c3 ...)
var __if = NewSyntax(_if, "if")
func _if(env *Env, test Any, exprs...Any) Any {
    var value = _void()
	switch len(exprs) {
	case 0:
		break
	case 1:
		if is, ok := test.(bool); !ok || is {
			return _eval(exprs[0], env)
		}
	case 2:
		if is, ok := test.(bool); !ok || is {
			return _eval(exprs[0], env)
		} else {
			return _eval(exprs[1], env)
		}
	default:
		return _if(env, exprs[2], exprs[3:]...)
	}
	return value
}

//func ___if() {
//}