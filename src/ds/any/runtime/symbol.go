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

func NewSymbol(s string) Symbol {
    return Symbol{name: s}
}

func (o Symbol) GetType() int {
    return TypeCodeSymbol
}

func (o Symbol) GetHash() uintptr {
    //return DsymbolZKZRstring(list1(o)).(Hasher).GetHash()
	return 0 // TODO
}

func (o Symbol) Equal(a Any) bool {
    if !_symbolZS(a).(bool) {
        return false
    }
    return o.name == a.(Symbol).name
}

func (o Symbol) Eval(env *Env) Any {
    value := env.Ref(o)
    if value == nil {
        _error("variable not bound in environment: ", o.name)
    }
    return value
}

func (o Symbol) Match(syntax Any, env *Env) bool {
    // TODO: if o isin literals, then return true
    if o.name == "_" { return true }
    if o.name == "..." {
        panic("we were supposed to catch ... earlier")
        return false
    }
    if value := env.Ref(o); value != nil && o.Equal(value) {
        return o.Equal(syntax)
    }
    //env.DefMatch(o, syntax)
    return true
}

func (o Symbol) Replace(env *Env) Any {
    value := env.Ref(o)
    if value == nil {
        return o
    }
    return value
}

func (sy Symbol) String() string {
    return sy.name
}

func (sy Symbol) SchemeString() string {
	return sy.String()
}

func (sy Symbol) ToString() String {
	return NewString(sy.String())
}

func (sy Symbol) ToVector() Vector {
	return sy.ToString().ToVector()
}
