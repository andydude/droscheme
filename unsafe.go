/*
 * Droscheme - a Scheme implementation
 * Copyright © 2012 Andrew Robbins, Daniel Connelly
 *
 * This program is free software: it is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
 */
package droscheme

import (
	//"os"
	//"fmt"
	"reflect"
	"runtime"
	"runtime/debug"
	//"unsafe"
)

type Cont uintptr
func GetCC(dummy *byte) Cont
func SetCC(dummy *byte, cont Cont)

//func GetCC() *Cont {
//	var dummy byte
//	return getCC(&dummy)
//}
//
//func SetCC(cc *Cont) {
//	var dummy byte
//	setCC(&dummy, cc)
//}



//func dumpstack(*byte, int32)
//func getstack(*byte, int32) []byte
//func getcontext(*byte) *byte
//func setcontext(*byte, *byte)
//func setcall(g uintptr)

func Dump() {
	//var dummy byte
	//dumpstack(&dummy, 100)
	debug.PrintStack()
}

func DumpInternals()

func GetFE(pc uintptr) uintptr {
	return runtime.FuncForPC(pc).Entry()
}

func GetFL(pc uintptr) (file string, line int) {
	return runtime.FuncForPC(pc).FileLine(pc)
}

func GetFN(pc uintptr) string {
	return runtime.FuncForPC(pc).Name()
}

func GetPC(fn interface{}) uintptr {
	return reflect.ValueOf(fn).Pointer()
}
/*
func OnceCC(cc chan Any, uc chan Any, proc Any) {
	fmt.Printf("--- enter OnceCC()\n", uc)
	var a Any
	select {
	case a = <- cc:
		fmt.Printf("Got(%v)\n", a)
		//if proc != nil {
		//	r := Dapply(list2(proc, a))
		//	fmt.Printf("Gives(%v)\n", r)
		//}
	}
	//var pc uintptr = uintptr(unsafe.Pointer(uc))
    //fmt.Printf("UseCC()UC=%p\n", uc)
    //fmt.Printf("UseCC()PC=0x%X\n", pc)
	//pc = GetPC(UseCC)
    //fmt.Printf("UseCC()PC=0x%X\n", pc)
    //fmt.Printf("UseCC()FN=%s\n", GetFN(pc))
	if uc != nil {
		SetCC(uc, a)
	}

	//var dummy byte
	//setcontext(&dummy, uc)
	fmt.Printf("--- leave OnceCC()\n")
	//os.Exit(0)
}

func EverCC(cc chan Any, uc chan Any, proc Any) {
	for {
		OnceCC(cc, uc, proc)
	}
}

func CallCC(proc Any) chan Any {
	fmt.Printf("--- enter GetCC()\n")
	cc := make(chan Any, 1)
	uc := make(chan Any, 1)

	if proc != nil {
		uc := GetCC(nil)
		go UseCC(cc, uc, proc)
		return cc
	}

	defer UseCC(uc, nil, nil)
	fmt.Printf("--- leave GetCC()\n")
	return cc
}

func ByeCC() {
	fmt.Printf("--- enter ByeCC()\n")
	cc := make(chan Any, 1)
	select {
	case _ = <- cc:
		panic("unreachable")
	}
	fmt.Printf("--- leave ByeCC()\n")
}
func SetCC(cc chan Any, a Any) {
	fmt.Printf("--- enter SetCC()\n")
	cc <- a
	ByeCC()
	fmt.Printf("--- leave SetCC()\n")
}
*/
