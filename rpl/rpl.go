package main

import (
	"bufio"
	"droscheme"
	"fmt"
	"os"
)

func Echo() {
	defer func(){
		if x := recover(); x != nil {
			fmt.Println("droscheme: caught exception:")
			fmt.Println(x)
		}
	}()

	//env := droscheme.BuiltinEnv()
	in := bufio.NewReader(os.Stdin)

	//L
	for line, rerr := droscheme.GetLine(in); rerr == nil; 
	    line, rerr = droscheme.GetLine(in) {

		//R
		val, lerr := droscheme.Read(line)
		if lerr != nil {
			fmt.Println(lerr)
			break
		}

		if val == nil {
			continue
		}

		//P
		if val.(fmt.Stringer).String() != "" {
			fmt.Println(val)
		}
	}

	fmt.Println()
}

func main() {
	Echo()
}
