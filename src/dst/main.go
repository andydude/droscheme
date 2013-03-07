package main

import (
	"strings"
	"runtime/debug"
	"io"
)

func isComplete(s string) bool {
	return strings.Count(s, "(") == strings.Count(s, ")")
}

func main() {
	var (
		input = standardZKinputZKport()
		output = standardZKoutputZKport()
		//env = interactionZKenvironment()
	)

	for {
		defer func(){
			err := recover()
			if err != nil {
				if err.(error).Error() == "end-of-line" {
					return
				}
				if err != io.EOF {
					debug.PrintStack()
				}
				print("\n---\n")
				panic(err)
			}
		}()
		print("\n> ")
		exp := read(input)
		//value := _eval(exp, env)
		value := exp
		write(value, output)
	}
}