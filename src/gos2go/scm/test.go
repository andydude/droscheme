package main 


import (
 "testing" 
)
 
func roundTripTestChans ( t * testing.T  )  {
	bio := __make ( chan int , 4 ) 
bo := __make ( chan<-! ( int ) , 4 ) 
bi := __make ( <-chan int , 4 ) 
bio <- 1 
bo <- 2 
fmt.Printf ( "%d", <- bi  ) 
fmt.Printf ( "%d", <- bio  ) 
}
  
func roundTripTestCase ( t * testing.T  )  {
	switch os.Args [ 0 ]  {
 case expr-list ( exprs )  : __print ( "What!" )  
  case expr-list ( exprs )  : __print ( "Realy?" )  
  default: __print ( "Wow" )  
  }
 
}
  
func roundTripTestCond ( t * testing.T  )  {
	switch {
 case expr : __print ( "Unlikely" )  
  default: __print ( "Probably" )  
  }
 
}
  
func roundTripTestComm ( t * testing.T  )  {
	io := __make ( chan int , 4 ) 
select {
 case expr : __print ( "sent" )  
  case expr : __print ( "recv" )  
  default: io <- 1  
  }
 
}
   