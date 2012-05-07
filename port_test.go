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

func TestPorts(t *testing.T) {
	if _, ok := DstandardZKerrorZKport(list0()).(TOPort); !ok {
		t.Error("stderr is not textual-output-port")
	}
	if _, ok := DstandardZKoutputZKport(list0()).(TOPort); !ok {
		t.Error("stdout is not textual-output-port")
	}
	if _, ok := DstandardZKinputZKport(list0()).(TIPort); !ok {
		t.Error("stdin is not textual-input-port")
	}
}