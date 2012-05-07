//
// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins, Daniel Connelly
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
//
package droscheme

import (
    "testing"
)

func typeStringOf(a Any) string {
	return typeToString(typeOf(a))
}

func aBinary(a string) Any { return NewBinary([]byte(a)) }
func aString(a string) Any { return NewString([]rune(a)) }
func aSymbol(a string) Any { return NewSymbol(a) }

func TestAny(t *testing.T) {
	if _, ok := aBinary("a").(Any); !ok {
		t.Error("Binary is not Any")
	}
	if _, ok := aString("a").(Any); !ok {
		t.Error("String is not Any")
	}
	if _, ok := aSymbol("a").(Any); !ok {
		t.Error("Symbol is not Any")
	}
}

func TestBinaryType(t *testing.T) {
	if typeStringOf(aBinary("a")) != "bytevector" {
		t.Error("Binary is not scheme:bytevector")
	}
	if typeStringOf(aString("a")) != "string" {
		t.Error("Binary is not scheme:bytevector")
	}
	if typeStringOf(aSymbol("a")) != "symbol" {
		t.Error("Binary is not scheme:bytevector")
	}
}
