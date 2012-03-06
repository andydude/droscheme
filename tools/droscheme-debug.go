package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"droscheme"
)

func perExpr(s string) {
	d := ReadDatumFromString(filename)
	d.WriteDebug()
}

func perFile(filename string) {
	var size int
	var err os.Error
	f := os.Open(filename)
	b := make([]byte, 4096)
	size, err = ReadFull(f, b)
	perExpr(string(b))
}

func main() {
	flag.Parse()
	for i := 0; i < flag.NArg(); i++ {
		perFile(flag.Arg(i))
	}
}