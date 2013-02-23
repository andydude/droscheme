package main

import (
//	"go/parser"
//	"go/printer"
//	"go/token"
	"flag"
	"os"
	"os/exec"
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
        return os.Create(*outputname)
    }()
    if err != nil {
        panic(err)
    }

	// find guile
	guile, err := exec.LookPath("guile")
	if err != nil {
		panic(err)
	}
	script := os.Getenv("DROSCHEME_PATH") + "/scm/scheme2gos-guile.scm"
	args := []string{"--no-auto-compile", "--debug", "-s", script}
	if *raw {
		args = append(args, "-r")
	}
	cmd := exec.Command(guile, args...)
	cmd.Stdin = rd
	cmd.Stderr = os.Stderr

	cmd.Stdout = wr
	cmd.Run()
	if *inputname != "-" {
		rd.Close()
	}
	if *outputname != "-" {
		wr.Close()
	}
}

func main() {
	flag.Parse()
	compile()
}