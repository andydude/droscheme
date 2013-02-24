package main

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io"
	"os"
	"strings"
)

type Syncer interface {
	Sync() error
}

type Buffer struct {
}

type Compiler struct {
//	br bufio.Reader
//	bw bufio.Writer
//	rd io.Reader
//	rw io.ReadWriter
	wr io.Writer
}

func NewBuffer() *Buffer {
	return &Buffer{}
}

func NewCompiler() *Compiler {
	return &Compiler{}
}

func (c *Compiler) Compile(rd io.Reader, wr io.Writer) (err error) {
	c.wr = wr
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "", rd, 0)
	if err != nil {
		return err
	}
	c.emitFile(file)
	if f, ok := c.wr.(io.Closer); ok {
		err = f.Close()
	}
	return
}

func (c *Compiler) compileFile(filename string) error {
	return c.compileFileTo(filename, os.Stdout)
}

func (c *Compiler) compileFileTo(filename string, wr io.Writer) error {
	rd, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	return c.Compile(rd, wr)
}

func (c *Compiler) compileString(input string) error {
	return c.Compile(strings.NewReader(input), os.Stdout)
}

func (c *Compiler) Visit(node ast.Node) (w ast.Visitor) {
	if a, ok := node.(ast.Decl); ok {
		c.emitDecl(a)
		return nil
	}
	return c
}

func goBinaryOpToSchemeOp(name string) string {
	var table = map[string]string{
		"&": "bitwise-and",
		"&&": "and",
		"&=": "bitwise-and=",
		"&^": "bitwise-but",
		"&^=": "bitwise-but=",
		"^": "bitwise-xor",
		"^=": "bitwise-xor=",
		"|": "bitwise-or",
		"|=": "bitwise-or=",
		"||": "or",
	}
	if table[name] != "" {
		return table[name]
	}
	return name
}

func goUnaryOpToSchemeOp(name string) string {
	var table = map[string]string{
		"&": "adr",
		"^": "bitwise-not",
		"!": "not",
		"*": "ptr",
	}
	if table[name] != "" {
		return table[name]
	}
	return name
}

func goIdToSchemeId(name string) string {
	var table = map[string]string{
		// types
		"bool": prefix+"bool",
		"byte": prefix+"byte",
		"complex64": prefix+"complex64",
		"complex128": prefix+"complex128",
		"error": prefix+"error",
		"float32": prefix+"float32",
		"float64": prefix+"float64",
		"int": prefix+"int",
		"int8": prefix+"int8",
		"int16": prefix+"int16",
		"int32": prefix+"int32",
		"int64": prefix+"int64",
		"rune": prefix+"rune",
		"string": prefix+"string",
		"uint": prefix+"uint",
		"uint8": prefix+"uint8",
		"uint16": prefix+"uint16",
		"uint32": prefix+"uint32",
		"uint64": prefix+"uint64",
		"uintptr": prefix+"uintptr",
		// builtins
		"append": prefix+"append",
		"cap": prefix+"cap",
		"close": prefix+"close",
		"complex": prefix+"complex",
		"copy": prefix+"copy",
		"delete": prefix+"delete",
		"imag": prefix+"imag",
		"len": prefix+"len",
		"panic": prefix+"panic",
		"print": prefix+"print",
		"println": prefix+"println",
		"real": prefix+"real",
		"recover": prefix+"recover",
		// objects
		"true": "#t",
		"false": "#f",
		"nil": prefix+"nil",
	}
	if table[name] != "" {
		return table[name]
	}
	return UnmangleName(name)
}

func goCharToSchemeChar(node *ast.BasicLit) string {
	buf := []rune(node.Value)
	if buf[1] == '\\' {
		switch buf[2] {
		case ' ': return "space"
		//case '0': return "nul"
		//case 'a': return "alert"
		//case 'b': return "backspace"
		//case 'f': return "formfeed"
		case 'a': return "bel"
		case 'b': return "backspace"
		case 'f': return "page"
		case 'n': return "linefeed"
		case 'r': return "return"
		case 't': return "tab"
		case 'v': return "vtab"
		case 'U':
		case 'u':
		case 'x':
		}
		return string(buf[2:len(buf)-1])
	}
	return string(buf[1:len(buf)-1])
}

func goStringToSchemeString(node *ast.BasicLit) string {
	if []byte(node.Value)[0] == '`' {
		v1 := node.Value
		v2 := strings.Replace(v1, "\"", "\\\"", -1)
		v3 := strings.Replace(v2, "`", "\"", -1)
		return v3
	}
	return node.Value
}

func MangleName(name string) string {
    const table = "!\"#$%&'*+,-./:;<=>?@\\^`|~Z"
    var out = []byte{}
    var work = []byte(name)
    for i := 0; i < len(work); i++ {
        ch := work[i]
        ix := strings.Index(table, string(ch))
        if ix != -1 {
            out = append(out, 'Z', 'A'+byte(ix))
        } else {
            out = append(out, ch)
        }
    }
    return string(out)
}

func UnmangleName(mangled string) string {
    const table = "!\"#$%&'*+,-./:;<=>?@\\^`|~Z"
    var out = []byte{}
    var work = []byte(mangled)
    for i := 0; i < len(work); i++ {
        ch := work[i]
        if ch == 'Z' {
            i++
            ch := work[i]
			ix := ch - 'A'
			if 0 <= ix && ix < byte(len(table)) {
				out = append(out, table[ch-'A'])
			} else {
				out = append(out, 'Z')
				out = append(out, ch)
			}
        } else {
            out = append(out, ch)
        }
    }
    return string(out)
}
