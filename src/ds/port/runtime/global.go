// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

package ds_port_runtime

import "os"

var (
	gStderr = NewFilePort(os.Stderr, "/dev/stderr", PortTypeCodeCharOut)
	gStdin = NewFilePort(os.Stdin, "/dev/stdin", PortTypeCodeCharIn)
	gStdout = NewFilePort(os.Stdout, "/dev/stdout", PortTypeCodeCharOut)
)
