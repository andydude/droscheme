package main

import (
	"bufio"
	"droscheme"
	"flag"
	"fmt"
	"os"
)

var gDontEval bool = false
var gEnv *droscheme.Env
var gExpr string = ""
var gFilename string = ""
var gInteract bool = false
var gMangle string = ""
var gShell bool = true
var gUnmangle string = ""

func arguments() {
	flag.BoolVar(&gDontEval, "d", false, "do not evaluate, just read and write")
	flag.StringVar(&gExpr, "e", "", "evaluate expr, then exit")
	flag.StringVar(&gFilename, "f", "", "evaluate file, then exit")
	flag.BoolVar(&gInteract, "i", false, "interactive mode (default)")
	flag.StringVar(&gMangle, "m", "", "mangle a scheme name")
	flag.StringVar(&gUnmangle, "u", "", "unmangle a go name")
	flag.Usage = usage
	flag.Parse()
}

func usage() {
	fmt.Printf("Usage of %s:\n"+
		"  -d       do not evaluate, just read and write\n"+
		"  -e EXPR  evaluate expr, then exit\n"+
		"  -f FILE  evaluate file, then exit\n"+
		"  -i       interactive mode (default)\n"+
		"  -m NAME  mangle a scheme name\n"+
		"  -u NAME  unmangle a go name\n", os.Args[0])
	os.Exit(2)
}

func process() {
	// TODO process script, not user input
}

func getLine(in *bufio.Reader, prompt string) (string, error) {
	fmt.Printf("%s> ", prompt)
	return in.ReadString('\n')
}

func shell() {
	defer func() {
		if x := recover(); x != nil {
			fmt.Println("droscheme: caught exception:")
			fmt.Println(x)
		}
	}()

	defaultPrompt := "ds"
	prompt := defaultPrompt
	lines := ""
	in := bufio.NewReader(os.Stdin)

	//L
	for line, rerr := getLine(in, prompt); rerr == nil; line, rerr = getLine(in, prompt) {

		if line == "\n" {
			lines = ""
			prompt = defaultPrompt
			continue
		} else {
			lines += line
		}

		if droscheme.CountParens(lines) != 0 {
			prompt = "   -"
			continue
		}

		//R
		val, lerr := droscheme.Read(lines)
		if lerr != nil {
			fmt.Println("ReadError: " + lerr.Error())
			continue
		}

		if val == nil {
			continue
		}

		lines = ""
		prompt = defaultPrompt

		if gDontEval {
			outs, ok := val.(fmt.Stringer)
			if ok && outs.String() != "" {
				fmt.Println(outs)
			}
			continue
		}

		//E
		out, verr := droscheme.Eval(val, gEnv)
		if verr != nil {
			fmt.Println("EvalError: " + verr.Error())
			continue
		}

		//P
		outs, ok := out.(fmt.Stringer)
		if ok && outs.String() != "" {
			fmt.Println(out)
		}
	}

	fmt.Println()
}

func main() {
	gInteract = true

	arguments()

	if gMangle != "" {
		fmt.Printf("%s\n", droscheme.MangleName(gMangle))
		os.Exit(0)
	}
	if gUnmangle != "" {
		fmt.Printf("%s\n", droscheme.UnmangleName(gUnmangle))
		os.Exit(0)
	}

	if gFilename != "" {
		gShell = false
	}

	if gExpr != "" {
		gShell = false
	}

	if gInteract {
		gShell = true
	}

	/* This creates a new environment that is empty
	 * but its parent is the builtin environment.
	 */
	gEnv = droscheme.ChildEnv(droscheme.BuiltinEnv())

	if gShell {
		shell()
	}
}
