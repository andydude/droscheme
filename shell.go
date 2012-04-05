/*
 * Droscheme - a Scheme implementation
 * Copyright © 2012 Andrew Robbins, Daniel Connelly
 *
 * This program is free software: it is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
 */
package droscheme

import (
)

// (ds builtin)
func BuiltinEnv() *Env {
	return DinteractionZKenvironment(list0()).(*Env)
}

// (eval expr env)
func Eval(expr Any, env *Env) (value Any, err error) {
	defer PanicToError(values0())
	return Deval(list2(expr, env)), nil
}

func Load(filename string, env *Env) (value Any, err error) {
	defer PanicToError(values0())
	return Dload(list2(ToString(filename), env)), nil
}