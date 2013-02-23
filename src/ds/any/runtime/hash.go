// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPL): <http://www.gnu.org/licenses/>.

package ds_any_runtime

import (
.	"ds/any"
	"bytes"
	"encoding/binary"
	"hash/crc32"
)

var objectZKhash = NewProc(_objectZKhash, "object-hash")
func _objectZKhash(rest ...Any) Any {
	if len(rest) == 1 {
		return rest[0].(Hasher).GetHash()
	}
	hashport := crc32.NewIEEE()
	for i := 0; i < len(rest); i++ {
		code := _u32ZKZRbytevector(_objectZKhash(rest[i]))
		_, err := hashport.Write([]byte(code.(Binary)))
		if err != nil {
			panic(err)
		}
	}
	return uintptr(hashport.Sum32())
}

var u32ZKZRbytevector = NewProc(_u32ZKZRbytevector, "u32->bytevector")
func _u32ZKZRbytevector(a Any) Any {
	buf := new(bytes.Buffer)
	err := binary.Write(buf, binary.LittleEndian, a)
	if err != nil {
		panic(err)
	}
	return Binary(buf.Bytes())
}
