// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

package ds_port

// TODO: s/PortTypeCode/Kind/g

// port type codes
const (
	KindByte_     = iota
	KindByteIn    // binary intput port
	KindByteOut   // binary output port
	KindByteInOut // binary port

	KindChar_
	KindCharIn    // textual input port
	KindCharOut   // textual output port
	KindCharInOut // textual port

	KindAny_
	KindAnyIn    // nonstandard, <-chan Any
	KindAnyOut   // nonstandard, chan<- Any
	KindAnyInOut // nonstandard, chan Any

	KindMax // maximum
)
