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
			// (1) check keyword bindings
			//if IsKeyword(first) {
			//	return Syntax(env.GetTransform(first), rest)
			//}

			// (2) check general bindings	
			// if we got here then it's not syntax
			return env.bound[expr.(SSymbol).name], nil
		}
		return expr, nil
	}

	// eval car and cdr
	cas, cds := unlist1R(expr)
	car, _ := EvalRec(cas, env, false) // starting a new list
	cdr, _ := EvalRec(cds, env, true) // continuation of this list

	if !recursive && IsProcedure(car) {
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
