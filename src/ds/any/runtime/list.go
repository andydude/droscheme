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

// null methods

func (_ *Null) GetType() int {
	return TypeCodeNull
}

func (_ *Null) GetHash() uintptr {
	return 0
}

func (_ *Null) Equal(a Any) bool {
	return _nullZS(a).(bool)
}

func (o *Null) Eval(env *Env) Any {
	return o
}

func (_ *Null) SchemeString() string {
	return "'()"
}

func (o *Null) ToVector() Any {
	return Vector([]Any{})
}

// pair methods

func (o *Pair) GetHash() uintptr {
	//return seqHash([]Any(_listZHZKZRvector(o).(Vector)))
	return 0 // TODO
}

func (o *Pair) GetType() int {
	return TypeCodePair
}

func (o *Pair) Equal(a Any) bool {
	if _, ok := a.(*Pair); !ok {
		return false
	}
	return _equalZS(o.car, _car(a)).(bool) && 
		   _equalZS(o.cdr, _cdr(a)).(bool)
}

func (o *Pair) Eval(env *Env) Any {
	v := []Any{}
	var cur Any
	for cur = o; _pairZS(cur).(bool); cur = cur.(*Pair).cdr {
		car := cur.(*Pair).car
		v = append(v, _eval(car, env))
	}
	return _vectorZKZRlist(Vector(v))
}
//
//func (p *Pair) Match(syntax Any, env *Env) bool {
//	pas, pds := _carZIcdr(p) // pattern
//
//	//fmt.Printf("List.Match[ %s, %s ]\n", p, syntax)
//
//	if _pairZS(pds).(bool) {
//		pads := pds.(*Pair).car
//		if _symbolZS(pads).(bool) && pads.(Symbol).String() == "..." {
//			//fmt.Printf("= ellipsis = %s\n", syntax)
//			if _symbolZS(pas).(bool) {
//				if env.Ref(pas) != nil {
//					//KdumpZKenvironment(pas, pds, env)
//					//fmt.Printf("= %s = %s\n", pas, env.Ref(pas))
//					//fmt.Printf("= %s | %s\n", p, syntax)
//					_error("list-match expected unbound symbol")
//				}
//				if _nullZS(syntax).(bool) || _pairZS(syntax).(bool) {
//					//env.DefMatch(pas, syntax)
//					//fmt.Printf("= #t 0\n")
//					return true
//				}
//				//fmt.Printf("= #f 4\n")
//				return false
//			}
//
//			// TODO
//			_error("ellipsis is only implemented for symbols")
//		}
//	}
//
//	cas, cds := _carZIcdr(syntax)
//
//	if !pas.(Matcher).Match(cas, env) {
//		//fmt.Printf("= #f 1\n")
//		return false
//	}
//	if !pds.(Matcher).Match(cds, env) {
//		//fmt.Printf("= #f 2\n")
//		return false
//	}
//	//fmt.Printf("= #t 3\n")
//	return true
//}
//
//func (t *Pair) Replace(env *Env) Any {
//	tas, tds := _carZIcdr(t)
//
//	if _pairZS(tds).(bool) {
//		tads := tds.(*Pair).car
//		if _symbolZS(tads).(bool) && tads.(Symbol).String() == "..." {
//			return env.Ref(tas)
//		}
//	}
//
//	car := tas.(Replacer).Replace(env)
//	cdr := tds.(Replacer).Replace(env)
//	return _cons(car, cdr)
//}
//
func (o *Pair) Ref(key Any) Any {
	return o.RefDefault(key, _void())
}

func (o *Pair) RefDefault(key, value Any) Any {
	if k, ok := key.(int); ok {
		if k == 0 {
			return o.car
		}
		return o.cdr.(Map).RefDefault(key, value)
	}
	_error("list-ref expected integer")
	return _void()
}

func (o *Pair) Set(key, value Any) {
	if k, ok := key.(int); ok {
		if k == 0 {
			o.SetFirst(value)
		}
		o.cdr.(Map).Set(key, value)
	}
	_error("list-set! expected integer")
}

func (o *Pair) SetFirst(value Any) {
	o.car = value
}

func (o *Pair) SetRest(value Any) {
	o.cdr = value
}

func (ls *Pair) SchemeString() string {
    if _listZS(ls).(bool) {
		// list
        v := _listZKZRvector(ls).(Vector)
        return v.SchemeString()[1:]
    }

	// list*
	v := _listZHZKZRvector(ls).(Vector)
	most := Vector(v[0:len(v) - 2]).SchemeString()
	last := _ZKZRschemeZKstring(v[len(v) - 1]).(string)
	return "(" + most[2:len(most) - 1] + " . " + last + ")"
}

func (o *Pair) ToVector() Any {
	var ret = []Any{}
	var cur Any
	for cur = o; _pairZS(cur).(bool); cur = cur.(*Pair).cdr {
		ret = append(ret, cur.(*Pair).car)
	}
	return Vector(ret)
}

func (o *Pair) First() Any {
	return o.car
}

func (o *Pair) Rest() Any {
	return o.cdr
}
