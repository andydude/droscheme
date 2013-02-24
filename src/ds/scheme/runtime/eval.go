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

var evalZKsyntax = NewSyntax(_evalZKsyntax, "eval-syntax")
func _evalZKsyntax(env *Env, exp Any) Any {
	return _eval(exp, env)
}

var eval = NewProc(_eval, "eval")
func _eval(exp Any, env Any) Any {
    if !_pairZS(exp).(bool) {
		return _evalZKliteral(exp, env)
    }

    // check if car is syntactic keyword
    cas, _ := _carZIcdr(exp)
    if _symbolZS(cas).(bool) {
        value := env.(*Env).Ref(cas)
        if trans, ok := value.(Syntax); ok {
            //fmt.Printf("--SYNTAX%s\n", exp)
            return trans.Transform(env.(*Env), exp)
        }
    }

    //fmt.Printf("--PROC%s\n", exp)
	return _evalZKvector(_listZKZRvector(exp), env.(*Env))
}

var evalZKliteral = NewProc(_evalZKliteral, "eval-literal")
func _evalZKliteral(exp Any, env Any) Any {
    // check for literals
    if _, ok := exp.(Evaler); !ok {
        return exp
    }

    return exp.(Evaler).Eval(env.(*Env))
}

var evalZKvector = NewProc(_evalZKvector, "eval-vector")
func _evalZKvector(exp Any, env Any) Any {

    // evaluate each argument
    arg := exp.(Vector).Eval(env.(*Env)).(Vector)

    // check if car is procedure
    car, cdr := arg[0], Vector(arg[1:])
    if !_procedureZS(car).(bool) {
        _error("expected procedure")
    }
    if _, ok := car.(Proc); !ok {
        _error("expected procedure (Applier)")
    }

    return car.(Proc).Apply(cdr)
}
