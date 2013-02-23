// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

package ds_any

// Any
//
//
type Any interface{}

// Evaler
//
//
type Evaler interface {
	Eval(env *Env) Any
}

// Named

// This interface represents maplets in environments.
//
// In order to lubricate the import and export between
// packages, this allows us to pass a single object to
// the environment which it can then query to find its
// name. After that, it may call Define() on the value.
type Named interface {
	Name() string
	Value() Any // of the type func(???)Any
}

// Proc
//
//
type Proc interface {
	Apply(args []Any) Any
}

// Syntax
//
// This interface represents macro transformers.
type Syntax interface {
	Transform(env *Env, exp Any) Any
}
