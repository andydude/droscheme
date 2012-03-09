package main

import (
	"bufio"
	"droscheme"
	"fmt"
	"os"
)

func getLine(in *bufio.Reader) (string, error) {
	fmt.Print(">> ")
	return in.ReadString('\n')
}

func main() {
	in := bufio.NewReader(os.Stdin)
	line, err := getLine(in)
	for err == nil {
		val, serr := droscheme.Read(line)
		if serr != nil {
			fmt.Println(serr)
		} else {
			fmt.Println(val)
		}
		line, err = getLine(in)
	}
	fmt.Println()
}
