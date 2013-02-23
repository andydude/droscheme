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

func goBinaryOpToXmlOp(name string) string {
	var table = map[string]string{
		"&": "<go:bitand/>",
		"&&": "<m:and/>",
		"&=": "<go:assign><go:bitand/></go:assign>",
		"&^": "<go:bitbut/>",
		"&^=": "<go:assign><go:bitbut/></go:assign>",
		"^": "<go:bitxor/>",
		"^=": "<go:assign><go:bitxor/></go:assign>",
		"|": "<go:bitor/>",
		"|=": "<go:assign><go:bitor/></go:assign>",
		"||": "<m:or/>",
		":=": "<go:var-is/>",
		"!=": "<m:neq/>",
		"==": "<m:eq/>",
		"<": "<m:lt/>",
		"<=": "<m:leq/>",
		">": "<m:gt/>",
		">=": "<m:geq/>",
		"+": "<m:plus/>",
		"-": "<m:minus/>",
		"+=": "<go:assign><m:plus/></go:assign>",
		"-=": "<go:assign><m:minus/></go:assign>",
		"*": "<m:times/>",
		"*=": "<go:assign><m:times/></go:assign>",
		"/": "<m:divide/>",
		"/=": "<go:assign><m:divide/></go:assign>",
		"%": "<m:rem/>",
		"%=": "<go:assign><m:rem/></go:assign>",
		"<<": "<go:shiftl/>",
		">>": "<go:shiftr/>",
		"<<=": "<go:assign><go:shiftl/></go:assign>",
		">>=": "<go:assign><go:shiftr/></go:assign>",
		"=": "<go:is/>",
	}
	if table[name] != "" {
		return table[name]
	}
	return name
}

func goUnaryOpToXmlOp(name string) string {
	var table = map[string]string{
		"&": "<go:adr/>",
		"^": "<go:bitnot/>",
		"!": "<m:not/>",
		"*": "<go:ptr/>",
		"++": "<go:inc/>",
		"--": "<go:dec/>",
	}
	if table[name] != "" {
		return table[name]
	}
	return name
}

func goIdToXmlId(name string) string {
	var table = map[string]string{
		// types
		"bool": "xsd:boolean",
		"byte": "go:byte",
		"complex64": "go:complex64",
		"complex128": "go:complex128",
		"error": "m:error",
		"float32": "xsd:float",
		"float64": "xsd:double",
		"int": "go:int",
		"int8": "xsd:byte",
		"int16": "xsd:short",
		"int32": "xsd:int",
		"int64": "xsd:long",
		"rune": "go:rune",
		"string": "xsd:string",
		"uint": "go:uint",
		"uint8": "xsd:unsignedByte",
		"uint16": "xsd:unsignedShort",
		"uint32": "xsd:unsignedInt",
		"uint64": "xsd:unsignedLong",
		"uintptr": "go:uintptr",
		// builtins
		"append": "go:append",
		"cap": "go:cap",
		"close": "go:close",
		"complex": "go:complex",
		"copy": "go:copy",
		"delete": "go:delete",
		"imag": "go:imag",
		"len": "go:len",
		"panic": "go:panic",
		"print": "go:print",
		"println": "go:println",
		"real": "go:real",
		"recover": "go:recover",
		// objects
		"true": "m:true",
		"false": "m:false",
		"nil": "go:nil",
	}
	if table[name] != "" {
		return "<" + table[name] + "/>"
	}
	return UnmangleName(name)
}

func goCharToXmlChar(node *ast.BasicLit) string {
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

func goStringToXmlString(node *ast.BasicLit) string {
	if []byte(node.Value)[0] == '`' {
		v1 := node.Value
		v2 := strings.Replace(v1, "\"", "\\\"", -1)
		v3 := strings.Replace(v2, "`", "\"", -1)
		return v3
	}
	//internalBuf := make([]byte, 1024)
	//buf := bytes.NewBuffer(internalBuf)
	//err := printer.Fprint(buf, token.NewFileSet(), node)
	//if err != nil {
	//	panic(err)
	//}
	if node.Value[0] == '"' {
		return node.Value[1:len(node.Value)-1]
	}
	if node.Value[0] == '\'' {
		return node.Value[1:len(node.Value)-1]
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
