package main

import "testing"

func roundTripTestChans(t *testing.T) {
	bio := make(chan int, 4)
	bo := make(chan<- int, 4)
	bi := make(<-chan int, 4)

	// do something
	bio <- 1
	bo <- 2

	// print something
	fmt.Printf("%d", <-bi)
	fmt.Printf("%d", <-bio)
}

func roundTripTestCase(t *testing.T) {
	switch os.Args[0] {
	case "/bin/sh":
		print("What!")
	case "/bin/bash":
		print("Realy?")
	default:
		print("Wow")
	}
}

func roundTripTestCond(t *testing.T) {
	switch {
	case t == nil:
		print("Unlikely")
	default:
		print("Probably")
	}
}

func roundTripTestComm(t *testing.T) {
	io := make(chan int, 4)
	select {
	case io <- 5:
		print("sent")
	case one := <-io:
		print("recv")
	default:
		io <- 1
	}
}

