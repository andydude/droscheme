// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

// Procedures
//
// This type represents Scheme procedures.
//
package ds_any

import "reflect"

// primitive procedure structure type
type PrimProc struct {
	call interface{} // not the same as any
	name string // scheme name
}

func NewProc(it Any, name string) *PrimProc {
	return &PrimProc{call: it, name: name}
}

func insToValues(as []Any, arity int, rest bool) (vs []reflect.Value) {
    vs = make([]reflect.Value, len(as))
    for i := 0; i < len(as); i++ {
        vs[i] = reflect.ValueOf(as[i])
    }
	return
}

func valuesToOuts(vs []reflect.Value) Any {
    if len(vs) == 1 {
        return vs[0].Interface().(Any)
    }
    //as := make([]Any, len(vs))
    //for i := 0; i < len(vs); i++ {
    //    as[i] = vs[i].Interface().(Any)
    //}
    //return NewValues(as)
	panic("primitive procedures must have 1 return value")
	return nil
}

func (pc *PrimProc) Apply(as []Any) Any {
    ov := reflect.ValueOf(pc.call)
    ot := reflect.TypeOf(pc.call)
	args := insToValues(as, ot.NumIn(), ot.IsVariadic())
	return valuesToOuts(ov.Call(args))
}

//func (pc *PrimProc) IsSyntax() bool {
//	return false
//}

func (pc *PrimProc) Name() string {
	return pc.name
}

func (pc *PrimProc) String() string {
	return pc.name
}

func (pc *PrimProc) Value() Any {
	return pc.call
}

// primitive syntax structure type
type PrimSyntax struct {
	call interface{} // not the same as any
	name string      // scheme name
	env *Env         // may be null
}

func NewSyntax(it Any, name string) *PrimSyntax {
	return &PrimSyntax{call: it, name: name}
}

func (kw *PrimSyntax) IsSyntax() bool {
	return true
}

func (kw *PrimSyntax) Name() string {
	return kw.name
}

func (kw *PrimSyntax) String() string {
	return kw.name
}

func (kw *PrimSyntax) Value() Any {
	return kw.call
}

