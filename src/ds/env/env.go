// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

// Environments
//
// This type is used for Scheme environments.
//
package ds_env

//import "fmt"
import (
	"fmt"
	"reflect"
	"runtime/debug"
)

type Env struct {
	it []map[string]interface{}
}

// Set value.Name()=value in current environment frame.
//
// This is primarily intended for initialization purposes.
func (env *Env) Add(name string, value interface{}) {
    env.it[0][value.Name()] = value
}

// Set name=NewProc(value, name) in current environment frame.
//
// This is primarily intended for initialization purposes.
func (env *Env) AddProc(value interface{}, name string) {
	// The symbol parameter must be a string.
    env.it[name] = NewProc(value, name)
}

// Set name=value in current environment frame.
//
// This is primarily intended for initialization purposes.
func (env *Env) AddValue(value interface{}, name string) {
    env.it[name] = value
}

//func (env *Env) newLambda(named_formals interface{}, body []interface{}) (id string, value interface{}) {
//	var (
//		_lambda = env.Ref("lambda").(Named).Value().(func(*Env, interface{}, ...interface{})interface{})
//		_pairZS = env.Ref("pair?").(Named).Value().(func(interface{})interface{})
//		_uncons = env.Ref("car+cdr").(Named).Value().(func(interface{})(interface{}, interface{}))
//	)
//
//	if !_pairZS(named_formals).(bool) {
//		panic("define expected pair but got " + 
//			named_formals.(fmt.Stringer).String())
//	}
//
//	symbol, formals := _uncons(named_formals)
//    id = symbol.(fmt.Stringer).String()
//	value = _lambda(env, formals, body...)
//    return
//}
//
//func (env *Env) AddLambda(named_formals interface{}, body []interface{}) {
//	symbol, value := env.newLambda(named_formals, body)
//	env.AddValue(symbol, value)
//}

// Set symbol=value in the current environment frame.
//
// The symbol parameter must be a string.
func (env *Env) Define(symbol, value interface{}) {
	env.defineString(symbol.(string), value)
}

func (env *Env) defineString(name string, value interface{}) {
    env.it[name] = value
}

// Make a new environment whose parent is this environment.
func (env *Env) Extend() *Env {
    return env.Update(NewEnv())
}

func (env *Env) Import(frame map[string]interface{}) *Env {
	child := NewEnv()

	for key, obj := range frame {
		value := reflect.ValueOf(obj)
		switch value.Kind() {
		case reflect.Func:
			// if arg1 is Env, then Syntax
			// else, then Proc
			child.AddProc(obj, key)
		default:
			fmt.Errorf("env.Import unrecognized type %v\n", obj)
		}
	}

    return env.Update(child)
}

// Get the symbol from this or any parent environment.
//
// The symbol parameter must be a string.
func (env *Env) Ref(symbol interface{}) interface{} {
	return env.refString(symbol.(string))
}

func (env *Env) refString(name string) interface{} {
	value := env.referString(name, nil)
	if value == nil {
		debug.PrintStack()
		panic("unbound symbol " + name)
	}
	return value
}

// Get the symbol from this or any parent environment, 
// with a devault value if not found.
//
// The symbol parameter must be a string.
func (env *Env) Refer(symbol, value interface{}) interface{} {
	return env.referString(symbol.(string), value)
}

func (env *Env) referString(name string, value interface{}) interface{} {
    if env.it[name] != nil {
        return env.it[name]
    }
    if env.parent != nil {
        return env.parent.referString(name, value)
    }
    return value
}

// Set symbol=value in the environment frame in which it is bound.
// If the symbol is not bound, then we throw an exception.
//
// The symbol parameter must be a string.
func (env *Env) Set(symbol, value interface{}) {
    name := symbol.(string)
	env.setString(name, value)
}

func (env *Env) setString(name string, value interface{}) {
    if env.it[name] != nil {
        env.it[name] = value
        return
    }
    if env.parent != nil {
        env.parent.setString(name, value)
		return
    }
    panic("set! expected bound variable")
}

// Make a new environment whose parent is this environment, where 
// the environment frame is populated with bindings from the most 
// recent environment frame associated with the child parameter.
func (env *Env) Update(child *Env) *Env {
    // child environment must have no parent,
    // but if it does, then it will be ignored
    return &Env{it: child.it, parent: env}
}


func (env *Env) AddEnv(child *Env) *Env {
    // child environment must have no parent,
    // but if it does, then it will be ignored
    return &Env{it: child.it, parent: env}
}

func (env *Env) AddEnvFrame(child map[string]interface{}) *Env {
    // child environment must have no parent,
    // but if it does, then it will be ignored
    return &Env{it: child.it, parent: env}
}
