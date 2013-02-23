
package main

import (
//	"bytes"
//	"fmt"
	"flag"
	"os"
	"os/exec"
	"runtime/pprof"
)

var debug = flag.Bool("d", false, "print debugging info")
var raw = flag.Bool("r", false, "print unformatted output")
var inputname = flag.String("i", "-", "input filename")
var outputname = flag.String("o", "-", "output filename")

func compile() {
	// open input file
	rd, err := func()(file *os.File, err error){
		if *inputname == "-" {
			return os.Stdin, nil
		}
		return os.Open(*inputname)
	}()
	if err != nil {
		panic(err)
	}

	// open output file
	wr, err := func()(file *os.File, err error){
		if *outputname == "-" {
			return os.Stdout, nil
		}
		return os.Open(*outputname)
	}()
	if err != nil {
		panic(err)
	}

	// find guile
	guile, err := exec.LookPath("guile")
	if err != nil || *raw {
		NewCompiler().Compile(rd, wr)
		rd.Close()
		return
	}

	// pretty-print
	const pretty = "(begin (use-modules (ice-9 pretty-print)) (pretty-print (read)))"
	cmd := exec.Command(guile, "-c", pretty)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	pr, err := cmd.StdinPipe()
	if err != nil {
		panic(err)
	}

	// compile to pipe
	NewCompiler().Compile(rd, pr)
	err = cmd.Run()
	if err != nil {
		panic(err)
	}

	rd.Close()
	//wr.Close()
}

func main() {
	flag.Parse()

	switch len(flag.Args()) {
	case 1:
		*inputname = flag.Arg(0)
	case 2:
		*inputname = flag.Arg(0)
		*outputname = flag.Arg(1)
	}

	if *debug {
		out, err := os.Create("profile")
		if err != nil { panic(err) }
		err = pprof.StartCPUProfile(out)
		if err != nil { panic(err) }
		compile()
		pprof.StopCPUProfile()
		out.Close()
	} else {
		compile()
	}
}