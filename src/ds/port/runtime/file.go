// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

package ds_port_runtime

import (
.	"ds/any"
	"ds/any/runtime"
	"fmt"
	"os"
	"unicode/utf8"
)

// file port

type FilePort struct {
	*os.File
	name string
	code int
	closed bool
	pos int64
}

func NewFilePort(file *os.File, filename string, code int) Any {
    port := &FilePort{name: filename, code: code}
    port.File = file
    return port
}

func (o *FilePort) Equal(a Any) bool {
	return false
}

func (o *FilePort) GetType() int {
	return ds_any_runtime.TypeCodePort
}

func (o *FilePort) GetHash() uintptr {
	return 0
}

func (o *FilePort) GetPortType() int {
	return o.code
}

func (o *FilePort) Flush() error {
	return o.Sync()
}

func (o *FilePort) String() string {
	return fmt.Sprintf("#<file-port:%s>", o.name)
}

func (o *FilePort) Peek(n int) (b []byte, err error) {
	return []byte{}, nil
}

func (o *FilePort) PeekByte() (b byte, err error) {
	defer o.UnreadByte()
	return o.ReadByte()
}

func (o *FilePort) PeekRune() (r rune, size int, err error) {
	defer o.UnreadRune()
	return o.ReadRune()
}

func (o *FilePort) ReadyByte() bool {
	return false // TODO
}

func (o *FilePort) ReadByte() (c byte, err error) {
	buf := make([]byte, 1)
	_, err =  o.Read(buf)
	if err != nil {
		c = 0
		return
	}
	return buf[0], nil
}

func (o *FilePort) ReadyRune() bool {
	return false // TODO
}

func (o *FilePort) ReadRune() (r rune, size int, err error) {
	buf := []byte{}
	for !utf8.FullRune(buf) {
		var c byte
		c, err = o.ReadByte()
		if err != nil {
			r = 0
			return
		}
		buf = append(buf, c)
	}
	ruf := []rune(string(buf))
	return ruf[0], len(buf), nil	
}

func (o *FilePort) ReadRunes(delim rune) (line []rune, err error) {
	return nil, nil
}

func (o *FilePort) UnreadByte() (err error) {
	o.pos, err = o.Seek(-1, 1)
	return
}

func (o *FilePort) UnreadRune() (err error) {
	return o.UnreadByte() // TODO
}

func (o *FilePort) WriteRunes(r []rune) (n int, err error) {
	return o.Write([]byte(string(r)))
}

func (o *FilePort) Closed() bool {
	return o.closed
}

