package main

import (
	"droscheme"
	"flag"
	"fmt"
	"strings"
)

func main() {
	flag.Parse()
	arg := flag.Arg(0)
	if strings.Index(arg, "Z") == -1 {
		fmt.Printf("%s\n", droscheme.MangleName(arg))
	} else {
		fmt.Printf("%s\n", droscheme.UnmangleName(arg))
	}
}
