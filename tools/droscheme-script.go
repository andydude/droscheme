package main

import (
	"flag"
	"droscheme"
)

func main() {
	var args = flag.Parse()
	// find out if we are in interactive mode
	droscheme.Shell(args)
	// if not do just run the script
	droscheme.Script(args)
}