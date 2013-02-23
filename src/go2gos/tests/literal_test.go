package main

import "testing"

func TestChans(t *testing.T) {
	
}

func roundTripTestBasicChar(t *testing.T) {
	fmt.Printf("%c", '\\')
	//fmt.Printf("%c", '\a') // WTF
	fmt.Printf("%c", '\b')
	fmt.Printf("%c", '\f')
	fmt.Printf("%c", '\n')
	fmt.Printf("%c", '\r')
	fmt.Printf("%c", '\t')
	//fmt.Printf("%c", '\v') // WTF
}

func roundTripTestBasicString(t *testing.T) {
	fmt.Printf("%s", "hello\nworld")
}
