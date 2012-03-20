package droscheme

import (
	"bufio"
	"fmt"
	"os"
	"reflect"
	"runtime"
	"strings"
//	"unsafe"
)

type Env struct {
	parent *Env
	bound map[string]Any
}

func NullEnv() *Env {
	return &Env{bound: make(map[string]Any, 1024)}
}

func ChildEnv(parent *Env) *Env {
	return &Env{bound: make(map[string]Any, 1024), parent: parent}
}

func (env *Env) get(id string) Any {
	if env.bound[id] != nil {
		return env.bound[id]
	}
	if env.parent == nil {
		return nil
	}
	return env.parent.get(id)
}

func (env *Env) has(id string) bool {
	if env.bound[id] != nil {
		return true
	}
	if env.parent == nil {
		return false
	}
	return env.parent.has(id)
}

func (env *Env) set(cds Any) (value Any, err error) {
	return env.mutate(cds, true)
}

func (env *Env) define(cds Any) (value Any, err error) {
	return env.mutate(cds, false)
}

func (env *Env) mutate(cds Any, bound bool) (value Any, err error) {
	bvar, sval := unlist2(cds)
	bval, derr := Eval(sval, env)
	id := bvar.(SSymbol).name
	if bound != env.has(id) {
		if bound {
			derr = newEvalError("set! variable must be prebound")
		} else {
			derr = newEvalError("define variable must be unbound")
		}
	}
	env.bound[id] = bval
	return values0(), derr
}

func (env *Env) registerName(fn interface{}) string {
	// intuit function name
    pc := reflect.ValueOf(fn).Pointer()
    name := runtime.FuncForPC(pc).Name()

	// strip package name
	list := strings.Split(name, ".")
	name = list[len(list) - 1]

	// strip first character
	return UnmangleName(name[1:])
}

func (env *Env) registerSyntax(fn func(Any, *Env) Any) {
	n := env.registerName(fn)
	env.bound[n] = SSyntax{form: fn, name: n}
}

func (env *Env) register(fn func(Any) Any) {
	n := env.registerName(fn)
	env.bound[n] = SProc{call: fn, name: n}
}

func MangleName(name string) string {
    const table = "!\"#$%&'*+,-./:;<=>?@^`|~Z"
    var out = []byte{}
    var work = []byte(name)
    for i := 0; i < len(work); i++ {
        ch := work[i]
        ix := strings.Index(table, string(ch))
        if ix != -1 {
            out = append(out, 'Z', 'A' + byte(ix))
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
            out = append(out, table[ch - 'A'])
        } else {
            out = append(out, ch)
        }
    }
    return string(out)
}

/* Apply()
 *
 * a call form is (<proc> <datum> ...)
 * which is given to this function as follows
 *
 * Apply( toAny(proc), toList(datum1, ...) )
 *
 * Note that the procedure is NOT included in 'args'.
 */
func Apply(proc, args Any) Any {
	return proc.(SProc).call(args)
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
	defer func(){
		rerr := recover()
		if rerr != nil {
			err = rerr.(error)
		}
	}()

	if !IsPair(expr) {
		panic("unreachable")
	}

	// check if car is syntactic keyword
	cas, _ := unlist1R(expr)
	// TODO: IsSyntax()
	if IsSymbol(cas) {
		value, serr := EvalSyntax(cas.(SSymbol).name, expr, env)
		if serr == nil {
			return value, nil
		}
	}

	// evaluate each argument
	list := expr.(SPair).Eval(env)

	// check if car is procedure
	car, cdr := unlist1R(list)
	if !IsProcedure(car) {
		panic("EvalError: expected procedure")
	}
	
	return Apply(car, cdr), nil
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
	defer func(){
		rerr := recover()
		if rerr != nil {
			value = values0()
			err = rerr.(error)
		}
	}()

	switch keyword {
	case "define":
		return Kdefine(expr, env), nil
	case "define-library":
		return Klibrary(expr, env), nil
	case "if":
		return Kif(expr, env), nil
	case "lambda":
		return Klambda(expr, env), nil
	case "library":
		return Klibrary(expr, env), nil
	case "quasiquote":
	case "quasisyntax":
	case "quote":
		return Kquote(expr, env), nil
	case "set!":
		return KsetZA(expr, env), nil
	case "syntax":
	case "unquote":
	case "unquote-splicing":
	case "unsyntax":
	case "unsyntax-splicing":
	}

	return values0(), newSyntaxError("unknown keyword")
}

func IsSyntax(keyword string, env *Env) bool {
	// TODO
	return true
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
	return env.bound[o.name]
}

func (o SVector) Eval(env *Env) Any {
	var ret = DmakeZKvector(list1(Sint64(len(o.items)))).(SVector)
	for i := 0; i < len(o.items); i++ {
		ret.items[i], _ = Eval(o.items[i], env)
	}
	return ret
}

//func EvalRec(expr Any, env *Env, recursive bool) (value Any, error evalError) {
//
//	// eval car and cdr
//	cas, cds := unlist1R(expr)
//
//	// (1) check keyword bindings
//	//if IsKeyword(cas) {
//	if IsSymbol(cas) {
//		//return Syntax(env.GetTransform(first), rest)
//	}
//
//	car, _ := EvalRec(cas, env, false) // starting a new list
//	cdr, _ := EvalRec(cds, env, true) // continuation of this list
//
//	if !recursive {
//		if !IsProcedure(car) {
//			panic("EvalError: expected procedure")
//		}
//		return Apply(car, cdr), nil
//	}
//
//	return list1R(car, cdr), nil
//}

type evalError *string

func GetLine(in *bufio.Reader) (string, error) {
	fmt.Print(">> ")
	return in.ReadString('\n')
}

func Shell() {
	defer func(){
		if x := recover(); x != nil {
			fmt.Println("droscheme: caught exception:")
			fmt.Println(x)
		}
	}()

	env := BuiltinEnv()
	in := bufio.NewReader(os.Stdin)

	//L
	for line, rerr := GetLine(in); rerr == nil; 
	    line, rerr = GetLine(in) {

		//R
		val, lerr := Read(line)
		if lerr != nil {
			fmt.Println(lerr)
			break
		}

		if val == nil {
			continue
		}

		//E
		out, verr := Eval(val, env)
		if verr != nil {
			fmt.Println(verr)
			break
		}

		//P
		if out.(fmt.Stringer).String() != "" {
			fmt.Println(out)
		}
	}

	fmt.Println()
}
