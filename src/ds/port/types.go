package ds_port

import (
.	"ds/any"
	"io"
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
