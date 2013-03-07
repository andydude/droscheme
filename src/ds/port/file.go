// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

package ds_port

import (
	"bufio"
	"ds/any"
	"fmt"
	"os"

//	"unicode/utf8"
)

// file port

type FilePort struct {
	fl     *os.File
	rd     *bufio.Reader
	wr     *bufio.Writer
	name   string
	kind   int
	closed bool
}

func (self *FilePort) Equal(a interface{}) bool {
	println("FilePort.Equal()")
	return false
}

func (self *FilePort) Kind() int {
	println("FilePort.Kind()")
	return ds_any.KindPort
}

func (self *FilePort) Hash() uintptr {
	println("FilePort.Hash()")
	return 1 // TODO
}

func (self *FilePort) Name() string {
	return self.name
}

func (self *FilePort) PortKind() int {
	println("FilePort.PortKind()")
	return self.kind
}

func (self *FilePort) Flush() error {
	println("FilePort.Flush()")
	err := self.wr.Flush()
	if err != nil {
		return err
	}
	err = self.fl.Sync()
	if err != nil {
		return err
	}
	return nil
}

func (self *FilePort) String() string {
	println("FilePort.String()")
	return fmt.Sprintf("#<file-port:%s>", self.name)
}

func (self *FilePort) Peek(n int) (b []byte, err error) {
	println("FilePort.Peek()")
	return self.rd.Peek(n)
}

func (self *FilePort) PeekByte() (b byte, err error) {
	defer self.rd.UnreadByte()
	println("FilePort.PeekByte()")
	return self.rd.ReadByte()
}

func (self *FilePort) PeekRune() (r rune, size int, err error) {
	defer self.rd.UnreadRune()
	println("FilePort.PeekRune()")
	return self.rd.ReadRune()
}

func (self *FilePort) ReadyByte() bool {
	println("FilePort.ReadyByte()")
	return self.rd.Buffered() > 0
}

func (self *FilePort) ReadByte() (c byte, err error) {
	println("FilePort.ReadByte()")
	return self.rd.ReadByte()
}

func (self *FilePort) ReadyRune() bool {
	println("FilePort.ReadyRune()")
	return self.rd.Buffered() > 0
}

func (self *FilePort) ReadRune() (r rune, size int, err error) {
	println("FilePort.ReadRune()")
	return self.rd.ReadRune()
}

func (self *FilePort) ReadRunes(delim rune) (line []rune, err error) {
	println("FilePort.ReadRunes()")
	return nil, nil
}

func (self *FilePort) UnreadByte() (err error) {
	println("FilePort.UnreadByte()")
	return self.rd.UnreadByte()
}

func (self *FilePort) UnreadRune() (err error) {
	println("FilePort.UnreadRune()")
	return self.rd.UnreadRune()
}

func (self *FilePort) Write(b []byte) (n int, err error) {
	println("FilePort.Write()")
	return self.wr.Write(b)
}

func (self *FilePort) WriteByte(c byte) error {
	println("FilePort.WriteByte()")
	return self.wr.WriteByte(c)
}

func (self *FilePort) WriteRune(r rune) (size int, err error) {
	println("FilePort.WriteRune()")
	return self.wr.WriteRune(r)
}

func (self *FilePort) WriteRunes(r []rune) (n int, err error) {
	println("FilePort.WriteRunes()")
	return self.wr.Write([]byte(string(r)))
}

func (self *FilePort) WriteString(s string) (n int, err error) {
	println("FilePort.WriteString()")
	return self.wr.WriteString(s)
}

func (self *FilePort) Closed() bool {
	println("FilePort.Closed()")
	return self.closed
}

func (self *FilePort) Close() error {
	println("FilePort.Close()")
	self.closed = true
	return self.fl.Close()
}
