//
// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins, Daniel Connelly
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
//
package droscheme

// null methods

func (o *Null) GetType() int {
	return TypeCodeNull
}

func (o *Null) GetHash() uintptr {
	return 0
}

func (_ *Null) Equal(a Any) bool {
	return _nullZS(a)
}

func (o *Null) Eval(env *Env) Any {
	return o
}

func (o *Null) Match(syntax Any, env *Env) bool {
	return o.Equal(syntax)
}

func (o *Null) Replace(env *Env) Any {
	return o
}

func (_ *Null) String() string {
	return "()"
}

func (o *Null) ToVector() Any {
	return Vector([]Any{})
}

// pair methods

func (o *Pair) GetHash() uintptr {
	return seqHash([]Any(DlistZHZKZRvector(list1(o)).(Vector)))
}

func (o *Pair) GetType() int {
	return TypeCodePair
}

func (o *Pair) Equal(a Any) bool {
	return Equal(o, a)
}

func (o *Pair) Eval(env *Env) Any {
	v := []Any{}
	var cur Any
	for cur = o; _pairZS(cur); cur = cur.(*Pair).cdr {
		car := cur.(*Pair).car
		v = append(v, Eval(car, env))
	}
	return DvectorZKZRlist(list1(NewVector(v)))
}

func (p *Pair) Match(syntax Any, env *Env) bool {
	pas, pds := unlist1R(p) // pattern

	//fmt.Printf("List.Match[ %s, %s ]\n", p, syntax)

	if _pairZS(pds) {
		pads := pds.(*Pair).car
		if _symbolZS(pads) && pads.(Symbol).String() == "..." {
			//fmt.Printf("= ellipsis = %s\n", syntax)
			if _symbolZS(pas) {
				if env.Ref(pas) != nil {
					KdumpZKenvironment(pas, pds, env)
					//fmt.Printf("= %s = %s\n", pas, env.Ref(pas))
					//fmt.Printf("= %s | %s\n", p, syntax)
					panic(newSyntaxError("list-match expected unbound symbol"))
				}
				if _nullZS(syntax) || _pairZS(syntax) {
					env.DefMatch(pas, syntax)
					//fmt.Printf("= #t 0\n")
					return true
				}
				//fmt.Printf("= #f 4\n")
				return false
			}

			// TODO
			panic(newSyntaxError("ellipsis is only implemented for symbols"))
		}
	}

	cas, cds := unlist1R(syntax)

	if !pas.(Matcher).Match(cas, env) {
		//fmt.Printf("= #f 1\n")
		return false
	}
	if !pds.(Matcher).Match(cds, env) {
		//fmt.Printf("= #f 2\n")
		return false
	}
	//fmt.Printf("= #t 3\n")
	return true
}

func (t *Pair) Replace(env *Env) Any {
	tas, tds := unlist1R(t)

	if _pairZS(tds) {
		tads := tds.(*Pair).car
		if _symbolZS(tads) && tads.(Symbol).String() == "..." {
			return env.Ref(tas)
		}
	}

	car := tas.(Replacer).Replace(env)
	cdr := tds.(Replacer).Replace(env)
	return list1R(car, cdr)
}

func (o *Pair) String() string {
	return DshowZKlist(list1(o)).(String).GoString()
}

func (o *Pair) ToVector() Any {
	var ret = []Any{}
	var cur Any
	for cur = o; _pairZS(cur); cur = cur.(*Pair).cdr {
		ret = append(ret, cur.(*Pair).car)
	}
	return NewVector(ret)
}

func (o *Pair) First() Any {
	return o.car
}

func (o *Pair) Rest() Any {
	return o.cdr
}
