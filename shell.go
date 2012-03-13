package droscheme

import (
	"bufio"
	"fmt"
	"os"
)

//func (env AnyDict) Read(source string) Any {
//	return env
//}
//
//func (env AnyDict) Eval(Any) Any {
//	return env
//}
//
//func Print(tree Syntax) {
//}

type Env struct {
	parent *Env
	bound map[string]Any
}

/* Apply()
 *
 * a call form is (<proc> <datum> ...)
 * which is given to this function as follows
 *
 * Apply( toAny(proc), toList(datum1, ...) )
 *
 * Note that the procedure is NOT included in the list.
 */
func Apply(proc, args Any) Any {
	//proc.(SProc).call(args.(SPair).car)
	return proc.(SProc).call(args)
}

/* Syntax()
 *
 * a syntax form is (<keyword> <datum> ...)
 * which is given to this function as follows
 *
 * Syntax( toTrans(keyword), toList(toSymbol(keyword), datum1, ...) )
 *
 * Note that the keyword is included in the list.
 */
func Syntax(trans, expr Any) Any {
	return list0()
}

func Eval(expr Any, env Env) (value Any, error evalError) {
	return EvalRec(expr, env, false) // starting a new list
}

func EvalRec(expr Any, env Env, recursive bool) (value Any, error evalError) {
	// handle constants first
	if !IsPair(expr) {
		switch t := expr.GetType(); {
		case t <= TypeCodeBool:
			return expr, nil
		case t == TypeCodeSymbol:
			// (2) check general bindings	
			// if we got here then it's not syntax
			n := expr.(SSymbol).name
			return env.bound[n], nil
		}
		return expr, nil
	}


	// eval car and cdr
	cas, cds := unlist1R(expr)

	// (1) check keyword bindings
	//if IsKeyword(cas) {
	if IsSymbol(cas) {
		switch n := cas.(SSymbol).name; {
		case n == "define":
			bvar, sval := unlist2(cds)
			bval, _ := EvalRec(sval, env, true)
			id := bvar.(SSymbol).name
			if env.bound[id] != nil {
				panic("EnvError: define variable must be unbound")
			}
			env.bound[id] = bval
			return list0(), nil

		case n == "define-library":
		case n == "if":
		case n == "lambda":
		case n == "library":
		case n == "quasiquote":
		case n == "quasisyntax":
		case n == "quote":
			return cds, nil
		case n == "set!":
			bvar, sval := unlist2(cds)
			bval, _ := EvalRec(sval, env, true)
			id := bvar.(SSymbol).name
			if env.bound[id] == nil {
				panic("EnvError: set! variable must be prebound")
			}
			env.bound[id] = bval
			return list0(), nil

		case n == "syntax":
		case n == "unquote":
		case n == "unquote-splicing":
		case n == "unsyntax":
		case n == "unsyntax-splicing":
		}
		//return Syntax(env.GetTransform(first), rest)
	}

	car, _ := EvalRec(cas, env, false) // starting a new list
	cdr, _ := EvalRec(cds, env, true) // continuation of this list

	if !recursive {
		if !IsProcedure(car) {
			panic("EvalError: expected procedure")
		}
		return Apply(car, cdr), nil
	}

	return list1R(car, cdr), nil
}

// this is defined in builtin.go
// because any changed to the list
// will have to be made in that file 
//func BuiltinEnv() Env {
//}

type evalError *string

func getLine(in *bufio.Reader) (string, error) {
	fmt.Print(">> ")
	return in.ReadString('\n')
}

func Shell() {
	defer func(){
		if x := recover(); x != nil {
			fmt.Println("droscheme: cought exception:")
			fmt.Println(x)
		} else {
			fmt.Println("droscheme: exited cleanly")
		}
	}()

	env := BuiltinEnv()
	globalCurrentEnv = env
	in := bufio.NewReader(os.Stdin)

	//L
	for line, rerr := getLine(in); rerr == nil; 
	    line, rerr = getLine(in) {

		//R
		val, lerr := Read(line)
		if lerr != nil {
			fmt.Println(lerr)
			break
		}

		//E
		out, verr := Eval(val, env)
		if verr != nil {
			fmt.Println(verr)
			break
		}

		//P
		fmt.Println(out)
	}

	fmt.Println()
}
