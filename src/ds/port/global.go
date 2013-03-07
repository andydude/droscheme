// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

package ds_port

import "os"

var (
	gStdInputPort  = openZKinputZKfile("/dev/stdin", os.Stdin)
	gStdOutputPort = openZKoutputZKfile("/dev/stdout", os.Stdout)
	gStdErrorPort  = openZKoutputZKfile("/dev/stderr", os.Stderr)
)
