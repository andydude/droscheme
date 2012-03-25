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
	"fmt"
	"reflect"
	"runtime"
	"strings"
)

type Env struct {
	parent *Env
	bound  map[string]Any
}

func NullEnv() *Env {
	return &Env{bound: make(map[string]Any, 1024)}
}

func ChildEnv(parent *Env) *Env {
	return &Env{bound: make(map[string]Any, 1024), parent: parent}
}

func (env *Env) GetType() int {
	return TypeCodeEnvSpec
}

func (env *Env) Equal(a Any) bool {
	return false
}

func (env *Env) Has(symbol Any) bool {
	id := symbol.(SSymbol).String()
	if env.bound[id] != nil {
		return true
	}
	if env.parent == nil {
		return false
	}
	return env.parent.Has(symbol)
}

func (env *Env) Ref(symbol Any) Any {
	id := symbol.(SSymbol).String()
	if env.bound[id] != nil {
		return env.bound[id]
	}
	if env.parent == nil {
		return nil
	}
	return env.parent.Ref(symbol)
}

func (env *Env) Set(symbol, value Any) Any {
	var id string
	var derr error = nil
	bvar, bval := symbol, value
	id = bvar.(SSymbol).name
	bval, derr = Eval(value, env)
	if !env.Has(symbol) {
		derr = newEvalError("set! variable must be prebound")
	}
	if derr == nil {
		env.bound[id] = bval
	} else {
		panic(derr)
	}
	return values0()
}

func (env *Env) Define(symbol, rest Any) Any {
	var id string
	var derr error = nil
	bvar, bval := symbol, rest
	if IsPair(bvar) {
		bid, form := unlist1R(bvar)
		id = bid.(SSymbol).name
		bval, derr = Eval(list2R(SSymbol{"lambda"}, form, bval), env)
	} else if IsSymbol(bvar) {
		id = bvar.(SSymbol).name
		bval, derr = Eval(list1R(SSymbol{"begin"}, bval), env)
	} else {
		derr = newEvalError("expected variable")
	}
	if env.Has(symbol) {
		derr = newEvalError("define variable must be unbound")
	}
	if derr == nil {
		env.bound[id] = bval
	} else {
		panic(derr)
	}
	return values0()
}

func (env *Env) registerName(fn interface{}) string {
	// intuit function name
	pc := reflect.ValueOf(fn).Pointer()
	name := runtime.FuncForPC(pc).Name()

	// strip package name
	list := strings.Split(name, ".")
	name = list[len(list)-1]

	// strip first character
	return UnmangleName(name[1:])
}

func (env *Env) registerSyntax(fn func(Any, *Env) Any) {
	n := env.registerName(fn)
	env.bound[n] = SSyntax{form: fn, name: n}
}

func (env *Env) register(fn func(Any) Any) {
	n := env.registerName(fn)
	env.bound[n] = SPrimProc{call: fn, name: n}
}

func MangleName(name string) string {
	const table = "!\"#$%&'*+,-./:;<=>?@^`|~Z"
	var out = []byte{}
	var work = []byte(name)
	for i := 0; i < len(work); i++ {
		ch := work[i]
		ix := strings.Index(table, string(ch))
		if ix != -1 {
			out = append(out, 'Z', 'A'+byte(ix))
		} else {
			out = append(out, ch)
		}
	}
	return string(out)
}

func UnmangleName(mangled string) string {
	const table = "!\"#$%&'*+,-./:;<=>?@^`|~Z"
	var out = []byte{}
	var work = []byte(mangled)
	for i := 0; i < len(work); i++ {
		ch := work[i]
		if ch == 'Z' {
			i++
			ch := work[i]
			out = append(out, table[ch-'A'])
		} else {
			out = append(out, ch)
		}
	}
	return string(out)
}

/* Eval()
 *
 * evaluates an expression
 */
func Eval(expr Any, env *Env) (value Any, err error) {
	switch {
	case IsPair(expr):
		return EvalList(expr, env)
	case IsSymbol(expr):
		return expr.(SSymbol).Eval(env), nil
	case IsVector(expr):
		return expr.(SVector).Eval(env), nil
	}
	return expr, nil
}

/* EvalPair()
 *
 * should only be called on CDR's
 */
func EvalPair(expr Any, env *Env) (value Any, err error) {
	switch {
	case IsNull(expr):
		return expr, nil
	case IsPair(expr):
		return expr.(SPair).Eval(env), nil
	case IsSymbol(expr):
		// might be an identifier bound to a list
		// as in (a b c . z) where z is a list
		return expr.(SSymbol).Eval(env), nil
	}

	// should we error on evaluating dotted lists?
	return expr, nil
}

/* EvalList()
 *
 * should only be called on full lists
 */
func EvalList(expr Any, env *Env) (value Any, err error) {
	defer func() {
		rerr := recover()
		if rerr != nil {
			err = ToError(rerr)
		}
	}()

	if !IsPair(expr) {
		panic("unreachable")
	}

	// check if car is syntactic keyword
	cas, _ := unlist1R(expr)
	// TODO: IsSyntax()
	if IsSymbol(cas) {
		keyword := cas.(SSymbol).name
		if IsSyntax(keyword, env) {
			fmt.Printf("--SYNTAX%s\n", expr)
			return EvalSyntax(keyword, expr, env)
		}
	}
	fmt.Printf("--PROC%s\n", expr)

	// evaluate each argument
	list := expr.(SPair).Eval(env)

	// check if car is procedure
	car, cdr := unlist1R(list)
	if !IsProcedure(car) {
		panic("EvalError: expected procedure")
	}

	return car.(Applier).Apply(cdr)
}

func ToError(a interface{}) error {
	switch a.(type) {
	case error:
		return a.(error)
	case string:
		return newEvalError(a.(string))
	}
	return newEvalError("unknown error")
}

/* EvalSyntax()
 *
 * a syntax form is (<keyword> <datum> ...)
 * which is given to this function as follows
 *
 * Syntax( toString(keyword), toList(toSymbol(keyword), datum1, ...), env )
 *
 * Note that the keyword is included in 'expr'.
 */
func EvalSyntax(keyword string, expr Any, env *Env) (value Any, err error) {
	defer func() {
		rerr := recover()
		if rerr != nil {
			value = values0()
			err = ToError(rerr)
		}
	}()

	if !IsSyntax(keyword, env) {
		return values0(), newSyntaxError("unknown keyword")
	}

	syntax := env.Ref(SSymbol{keyword})
	return syntax.(SSyntax).form(expr, env), nil
}

func IsSyntax(keyword string, env *Env) bool {
	if env.Has(SSymbol{keyword}) && IsType(env.Ref(SSymbol{keyword}), TypeCodeSyntax) {
		return true
	}
	return false
}

func CountParens(s string) int {
	return strings.Count(s, "(") - strings.Count(s, ")")
}

func (o SPair) Eval(env *Env) Any {
	cas, cds := unlist1R(o)
	car, err := Eval(cas, env)
	if err != nil {
		panic(err)
	}
	cdr, err := EvalPair(cds, env)
	if err != nil {
		panic(err)
	}
	return list1R(car, cdr)
}

func (o SSymbol) Eval(env *Env) Any {
	// (2) check general bindings
	// if we got here then it's not syntax
	return env.Ref(o)
}

func (o SVector) Eval(env *Env) Any {
	var ret = DmakeZKvector(list1(Sint64(len(o.it)))).(SVector)
	for i := 0; i < len(o.it); i++ {
		ret.it[i], _ = Eval(o.it[i], env)
	}
	return ret
}
