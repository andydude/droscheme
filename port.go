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
	"fmt"
	"io"
	"os"
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
 * ReadyByte() bool                         // ds.BytePeeker
 * ReadyRune() bool                         // ds.RunePeeker
 *
 * -- All output-ports must implement:
 * Flush() error                            // ds.Flusher
 * Write(p []byte) (n int, err error)       // io.Writer
 * WriteRunes(p []rune) (n int, err error)  // ds.RunesWriter -- assume UTF-8
 *
 */


// abstract interfaces

type Closer interface {
	io.Closer
	Closed() bool
}

// implemented by bufio.Writer
type Flusher interface {
	Flush() error
}

// implemented by bufio.Reader
type Peeker interface {
	Peek(n int) (c []byte, err error)
}

type BytePeeker interface {
	PeekByte() (c byte, err error)
    ReadyByte() bool
}

type RunePeeker interface {
	PeekRune() (r rune, size int, err error)
    ReadyRune() bool
}

// similar to io.WriteString
type RunesWriter interface {
	WriteRunes(p []rune) (n int, err error)
}

type LineReader interface {
	ReadLine() (line []rune, err error)
}

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
	RunePeeker
	LineReader
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

// (open-binary-input-file f)
func OpenBIFile(filename string) Any {
	f, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	return SFilePort{f, filename, PortTypeCodeByteIn}
}

// (open-binary-output-file f)
func OpenBOFile(filename string) Any {
	f, err := os.Create(filename)
	if err != nil {
		panic(err)
	}
	return SFilePort{f, filename, PortTypeCodeByteOut}
}

// (open-input-file f)
func OpenTIFile(filename string) Any {
	f, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	return SFilePort{f, filename, PortTypeCodeCharIn}
}

// (open-output-file f)
func OpenTOFile(filename string) Any {
	f, err := os.Create(filename)
	if err != nil {
		panic(err)
	}
	return SFilePort{f, filename, PortTypeCodeCharOut}
}

// file port

type SFilePort struct {
	*os.File
	name string
	code int
}

func (o SFilePort) Equal(a Any) bool {
	return false
}

func (o SFilePort) GetType() int {
	return TypeCodePort
}

func (o SFilePort) GetHash() uintptr {
	return 0
}

func (o SFilePort) GetPortType() int {
	return o.code
}

func (o SFilePort) Flush() error {
	return o.Sync()
}

func (o SFilePort) String() string {
	return fmt.Sprintf("#<file-port:%s>", o.name)
}

func (o SFilePort) ReadyByte() bool {
	return false // TODO
}

func (o SFilePort) ReadByte() (c byte, err error) {
	buf := make([]byte, 1)
	_, err =  o.Read(buf)
	if err != nil {
		c = 0
		return
	}
	return buf[0], nil
}

func (o SFilePort) ReadyRune() bool {
	return false // TODO
}

func (o SFilePort) ReadRune() (r rune, size int, err error) {
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

func (o SFilePort) WriteRunes(r []rune) (n int, err error) {
	return o.Write([]byte(string(r)))
}

//func (o SFilePort) ReadByte() (c byte, err error) {
//	c = 0
//	return
//}
//
//func (o SFilePort) ReadRune() (r rune, err error) {
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
	return []byte{}, nil
}

func (o *SBytePort) PeekByte() (b byte, err error) {
	return 0, nil
}

func (o *SBytePort) PeekRune() (r rune, err error) {
	return 0, nil
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
	return 0, nil
}

func (o *SCharPort) PeekRune() (r rune, err error) {
	return 0, nil
}

func (o *SCharPort) Read(p []byte) (n int, err error) {
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
	return 0, nil
}

func (o *SCharPort) ReadRune() (r rune, size int, err error) {
	if o.pos >= len(o.it) {
		return 0, 0, io.EOF
	}
	r = o.it[o.pos]
	o.pos++
	size = len([]byte(string([]rune{r})))
	err = nil
	return
}

func (o *SCharPort) ReadLine() (line []rune, err error) {
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

func (o *SCharPort) Write(c []byte) (n int, err error) {
	// TODO: recalculate length in bytes
	return o.WriteRunes([]rune(string(c)))
}

func (o *SCharPort) WriteRunes(r []rune) (n int, err error) {
	o.it = append(o.it, r...)
	return len(r), nil
}

