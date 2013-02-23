package main

import (
    "testing"
)

func TestChans(t *testing.T) {
	
}

func roundTripTestChans(t *testing.T) {
	bio := make(chan int, 4)
	bi := make(chan<- int, 4)
	bo := make(<-chan int, 4)

	// do something
	bio <- 1
	bi <- 2

	// print something
	fmt.Printf("%d", (<- bo))
	fmt.Printf("%d", (<- bio))
}