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
	"fmt"
	"io"
	"os"
//	"runtime/debug"
	"unicode/utf8"
)

/*
 * -- All ports must implement:
 * Any
 * GetPortType() int                        // ds.Port
 * Close() error                            // io.Closer
 * Closed() bool                            // ds.Closer
 *
 * -- All input-ports must implement:
 * Peek(n int) (c []byte, err error)        // ds.Peeker
 * PeekByte() (c byte, err error)           // ds.BytePeeker
 * PeekRune() (r rune, size int, err error) // ds.RunePeeker -- assume UTF-8
 * Read(p []byte) (n int, err error)        // io.Reader
 * ReadByte() (c byte, err error)           // io.ByteReader
 * ReadRune() (r rune, size int, err error) // io.RuneReader -- assume UTF-8
 * ReadLine() (line []rune, err error)      // ds.LineReader
 * ReadRunes(delim rune) ([]rune, error)    // ds.RunesReader
 * ReadyByte() bool                         // ds.BytePeeker
 * ReadyRune() bool                         // ds.RunePeeker
 * UnreadByte() error                       // io.ByteScanner
 * UnreadRune() error                       // io.RuneScanner
 *
 * -- All output-ports must implement:
 * Flush() error                            // ds.Flusher
 * Write(p []byte) (n int, err error)       // io.Writer
 * WriteByte(c byte) error                  // ds.ByteWriter
 * WriteRune(r rune) (size int, err error)  // ds.RuneWriter
 * WriteRunes(p []rune) (n int, err error)  // ds.RunesWriter -- assume UTF-8
 * WriteString(s string) (int, error)       // ds.StringWriter
 *
 */


// abstract interfaces

type Closer interface {
	io.Closer
	Closed() bool
}

// implemented by dsbufio.Writer
type Flusher interface {
	Flush() error
}

// implemented by dsbufio.Reader
type Peeker interface {
	Peek(n int) (c []byte, err error)
}

type BytePeeker interface {
	PeekByte() (c byte, err error)
    ReadyByte() bool
	UnreadByte() error
}

type RunePeeker interface {
	PeekRune() (r rune, size int, err error)
    ReadyRune() bool
	UnreadRune() error
}

type LineReader interface {
	ReadLine() (line []byte, isPrefix bool, err error)
}

type StringReader interface {
	ReadString(delim byte) (line string, err error)
}

type RunesReader interface {
	ReadRunes(delim rune) (line []rune, err error)
}

// similar to io.WriteString
type RunesWriter interface {
	WriteRunes(p []rune) (n int, err error)
}

//type LineReader interface {
//	ReadLine() (line []rune, err error)
//}

// scheme-specific interfaces

type BIPort interface {
	io.ByteReader
	io.Reader
	BytePeeker
	Peeker
}

type BOPort interface {
	io.Writer
}

type TIPort interface {
	io.RuneReader
	RunesReader
	RunePeeker
}

type TOPort interface {
	RunesWriter
}

type BPort interface {
	BIPort
	BOPort
}

type TPort interface {
	TIPort
	TOPort
}

type IPort interface {
	BIPort
	TIPort
}

type OPort interface {
	BOPort
	TOPort
	Flusher
}

type Port interface {
	Any
	GetPortType() int
	Closer
}

func IsPort(o Any) bool {
	var _, ok = o.(Port)
	return ok
}

func IsBinaryPort(o Any) bool {
	var p, ok = o.(Port)
	if !ok {
		return false
	}
	var t = p.GetPortType()
	if t > PortTypeCodeByteInOut {
		return false
	}
	return true
}

func IsTextualPort(o Any) bool {
	var p, ok = o.(Port)
	if !ok {
		return false
	}
	var t = p.GetPortType()
	if t > PortTypeCodeCharInOut {
		return false
	}
	if t < PortTypeCodeChar {
		return false
	}
	return true
}

func IsInputPort(o Any) bool {
	var p, ok = o.(Port)
	if !ok {
		return false
	}
	var t = p.GetPortType()
	if t&PortTypeCodeByteIn == 0 {
		return false
	}
	return true
}

func IsOutputPort(o Any) bool {
	var p, ok = o.(Port)
	if !ok {
		return false
	}
	var t = p.GetPortType()
	if t&PortTypeCodeByteOut == 0 {
		return false
	}
	return true
}

func OpenBIPort(input string) Any {
	return DopenZKinputZKbytevector(list1(ToBinary(input)))
}

func OpenTIPort(input string) Any {
	return DopenZKinputZKstring(list1(ToString(input)))
}

func newFile(file *os.File, filename string, code int) Any {
	port := &FilePort{name: filename, code: code}
	port.File = file
	return port
}

func err2File(f *os.File, err error) *os.File {
	if err != nil {
		panic(err)
	}
	return f
}

// (open-binary-input-file f)
func OpenBIFile(filename string) Any {
	return newFile(err2File(os.Open(filename)), filename, PortTypeCodeByteIn)
}

// (open-binary-output-file f)
func OpenBOFile(filename string) Any {
	return newFile(err2File(os.Create(filename)), filename, PortTypeCodeByteOut)
}

// (open-input-file f)
func OpenTIFile(filename string) Any {
	return newFile(err2File(os.Open(filename)), filename, PortTypeCodeCharIn)
}

// (open-output-file f)
func OpenTOFile(filename string) Any {
	return newFile(err2File(os.Create(filename)), filename, PortTypeCodeCharOut)
}

// file port

type FilePort struct {
	*os.File
	name string
	code int
	closed bool
	pos int64
}

func (o *FilePort) Equal(a Any) bool {
	return false
}

func (o *FilePort) GetType() int {
	return TypeCodePort
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

//func (o *FilePort) ReadByte() (c byte, err error) {
//	c = 0
//	return
//}
//
//func (o *FilePort) ReadRune() (r rune, err error) {
//	r = 0
//	return
//}

// bytevector/binary port

type SBytePort struct {
	it SBinary
	code int
	pos int
	closed bool
}

func (o *SBytePort) Equal(a Any) bool {
	return false
}

func (o *SBytePort) GetHash() uintptr {
	return 0
}

func (o *SBytePort) GetType() int {
	return TypeCodePort
}

func (o *SBytePort) GetPortType() int {
	return o.code
}

func (o *SBytePort) Closed() bool {
	return o.closed
}

func (o *SBytePort) Close() error {
	o.closed = true
	return nil
}

func (o *SBytePort) Peek(n int) (b []byte, err error) {
	if o.pos >= len(o.it) {
		return nil, io.EOF
	}
	return []byte{}, nil
}

func (o *SBytePort) PeekByte() (b byte, err error) {
	defer o.UnreadByte()
	return o.ReadByte()
}

func (o *SBytePort) PeekRune() (r rune, n int, err error) {
	defer o.UnreadRune()
	return o.ReadRune()
}

func (o *SBytePort) Read(p []byte) (n int, err error) {
	if o.pos >= len(o.it) {
		return 0, io.EOF
	}
	for n = 0; n < len(p) && err == nil; n++ {
		p[n], err = o.ReadByte()
	}
	if err == io.EOF {
		return n, nil
	}
	return
}

func (o *SBytePort) ReadByte() (c byte, err error) {
	if o.pos >= len(o.it) {
		return 0, io.EOF
	}
	c = o.it[o.pos]
	o.pos++
	err = nil
	return
}

func (o *SBytePort) ReadRune() (r rune, size int, err error) {
	buf := []byte{}
	for !utf8.FullRune(buf) {
		var c byte
		c, err = o.ReadByte()
		if err != nil {
			size = 0
			r = 0
			return
		}
		buf = append(buf, c)
	}
	ruf := []rune(string(buf))
	return ruf[0], len(buf), nil
}

func (o *SBytePort) ReadRunes(delim rune) (line []rune, err error) {
	return nil, nil
}

func (o *SBytePort) ReadLine() (line []rune, err error) {
	if o.pos >= len(o.it) {
		return []rune{}, io.EOF
	}
	var r rune
	var buf []rune
	for buf = []rune{}; err == nil && r != '\n'; buf = append(buf, r) {
		r, _, err = o.ReadRune()
	}
	return buf, nil
}

func (o *SBytePort) UnreadByte() error {
	o.pos--
	return nil
}

func (o *SBytePort) UnreadRune() error {
	o.pos--
	return nil
}

// string/textual port

type SCharPort struct {
	it SString
	code int
	pos int
	closed bool
}

func (o *SCharPort) Equal(a Any) bool {
	return false
}

func (o *SCharPort) GetHash() uintptr {
	return 0
}

func (o *SCharPort) GetType() int {
	return TypeCodePort
}

func (o *SCharPort) GetPortType() int {
	return o.code
}

func (o *SCharPort) Closed() bool {
	return o.closed
}

func (o *SCharPort) Close() error {
	o.closed = true
	return nil
}

func (o *SCharPort) Flush() error {
	return nil
}

func (o *SCharPort) Peek(n int) (b []byte, err error) {
	return []byte{}, nil
}

func (o *SCharPort) PeekByte() (b byte, err error) {
	//fmt.Printf("--- StringPort.PeekByte()\n")
	b, err = o.ReadByte()
	err = o.UnreadByte()
	return
}

func (o *SCharPort) PeekRune() (r rune, size int, err error) {
	//fmt.Printf("--- StringPort.PeekRune()\n")
	r, size, err = o.ReadRune()
	error1panic(o.UnreadRune())
	return
}

func (o *SCharPort) Read(p []byte) (n int, err error) {
	//fmt.Printf("--- StringPort.Read()\n")
	if o.pos >= len(o.it) {
		return 0, io.EOF
	}
	var r rune
	var width int
	for n = 0; n < len(p) && err == nil; n += width {
		r, width, err = o.ReadRune()
		buf := []byte(string([]rune{r}))
		copy(p[n:], buf)
	}
	if err == io.EOF {
		return n, nil
	}
	return
}

func (o *SCharPort) ReadByte() (c byte, err error) {
	//fmt.Printf("--- StringPort.ReadByte()\n")
	return 0, newTypeError("u8-read expected binary-port")
}

func (o *SCharPort) ReadyByte() bool {
	//fmt.Printf("--- StringPort.ReadyByte()\n")
	return true
}

func (o *SCharPort) ReadRune() (r rune, size int, err error) {
	//debug.PrintStack()
	//fmt.Printf("--- StringPort.ReadRune()\n")
	if o.pos >= len(o.it) {
		return 0, 0, io.EOF
	}
	r = o.it[o.pos]
	o.pos++
	size = len([]byte(string([]rune{r})))
	err = nil
	return
}

func (o *SCharPort) ReadyRune() bool {
	//fmt.Printf("--- StringPort.ReadyRune()\n")
	return true
}

func (o *SCharPort) ReadRunes(delim rune) (line []rune, err error) {
	return nil, nil
}

func (o *SCharPort) ReadLine() (line []rune, err error) {
	//fmt.Printf("--- StringPort.ReadLine()\n")
	if o.pos >= len(o.it) {
		return []rune{}, io.EOF
	}
	var r rune
	var buf []rune
	for buf = []rune{}; err == nil && r != '\n'; buf = append(buf, r) {
		r, _, err = o.ReadRune()
	}
	return buf, nil
}

func (o *SCharPort) UnreadByte() error {
	//fmt.Printf("--- StringPort.UnreadByte()\n")
	o.pos--
	return nil
}

func (o *SCharPort) UnreadRune() error {
	//fmt.Printf("--- StringPort.UnreadRune()\n")
	o.pos--
	return nil
}

func (o *SCharPort) Write(c []byte) (n int, err error) {
	//fmt.Printf("--- StringPort.Write()\n")
	// TODO: recalculate length in bytes
	return o.WriteRunes([]rune(string(c)))
}

func (o *SCharPort) WriteRunes(r []rune) (n int, err error) {
	o.it = append(o.it, r...)
	return len(r), nil
}

