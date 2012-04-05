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

// abstract interfaces

// similar to io.WriteString
type RunesWriter interface {
	WriteRunes(p []rune) (n int, err error)
}

// scheme-specific interfaces

type BIPort interface {
	io.ByteReader
}

type BOPort interface {
	io.Writer
}

type TIPort interface {
	io.RuneReader
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
}

type Port interface {
	Any
	GetPortType() int
	io.Closer
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

func (o SFilePort) GetPortType() int {
	return o.code
}

func (o SFilePort) String() string {
	return fmt.Sprintf("#<file-port:%s>", o.name)
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
}

func (o SBytePort) GetType() int {
	return TypeCodePort
}

func (o SBytePort) Equal(a Any) bool {
	return false
}


// string/textual port

type SCharPort struct {
	it SString
	code int
}

func (o SCharPort) GetType() int {
	return TypeCodePort
}

func (o SCharPort) Equal(a Any) bool {
	return false
}
