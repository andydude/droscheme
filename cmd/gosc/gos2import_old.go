package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"strings"
)

var gFilename string
var gFilebase string
var gSignatures map[string]string

func MangleName(name string) string {
    const table = "!\"#$%&'*+,-./:;<=>?@^`|~Z"
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
    const table = "!\"#$%&'*+,-./:;<=>?@^`|~Z"
    var out = []byte{}
    var work = []byte(mangled)
    for i := 0; i < len(work); i++ {
        ch := work[i]
        if ch == 'Z' {
            i++
            ch := work[i]
            out = append(out, table[ch-'A'])
        } else {
            out = append(out, ch)
        }
    }
    return string(out)
}

type Visitor struct{}

func (v Visitor) Visit(node ast.Node) (w ast.Visitor) {
	if node == nil {
		return nil
	}
	switch node.(type) {

	case *ast.CallExpr:
		expr := node.(*ast.CallExpr)
		if pair, ok := expr.Fun.(*ast.SelectorExpr); ok {
			if env, ok := pair.X.(*ast.Ident); ok {
				if env.String() != "env" {
					return v
				}
			} else {
				return v
			}
			switch pair.Sel.String() {
			case "registerGos": // AddProc in future
				mangled := expr.Args[0].(*ast.Ident).String()
				// TODO
				mangled = strings.TrimLeft(mangled, "_")
				name := UnmangleName(mangled)
				// TODO
				typetext := "func()Any"
				fmt.Printf("var %s = _%s.Ref(\"%s\")\n", mangled, gFilebase, name)
				fmt.Printf("var _%s = %s.(*Proc).call.(%s)\n", mangled, mangled, typetext)
			default:
				return v
			}
		}
		//fmt.Printf("CallExpr %v\n", expr)

	case *ast.FuncDecl:
		decl := node.(*ast.FuncDecl)
		fstr := decl.Name.String()
		if []byte(fstr)[0] != '_' {
			return v
		}
		inum := decl.Type.Params.NumFields()
		onum := decl.Type.Results.NumFields()
		buf := []byte{}
		buf = append(buf, "func "...)
		switch inum {
		case 0:
			buf = append(buf, "()"...)
		case 1:
			buf = append(buf, "(Any)"...)
		default:
			buf = append(buf, "(Any"...)
			for i := 1; i <= inum; i++ {
				buf = append(buf, ", Any"...)
			}
			buf = append(buf, ")"...)
		}
		switch onum {
		case 1:
			buf = append(buf, " Any"...)
		case 2:
			buf = append(buf, " (Any, Any)"...)
		default:
			panic("multiple returns" + decl.Name.String())
		}
		mangled := strings.TrimLeft(decl.Name.String(), "_")
		gSignatures[mangled] = string(buf)
		//fmt.Printf("FuncDecl %s (%s)\n", mangled, string(buf))
	}
	return v
}

func main() {
	gFilename = os.Args[1]
	gFilebase = strings.Split(gFilename, "/")[len(strings.Split(gFilename, "/")) - 1]
	gFilebase = strings.Split(gFilebase, ".")[0]
	gSignatures = make(map[string]string, 16)

	gos := string([]byte(gFilebase)[len(gFilebase)-4:len(gFilebase)])
	if gos == "_gos" {
		gFilebase = string([]byte(gFilebase)[0:len(gFilebase)-4])
	}

	// important environment
	fmt.Printf("var _%s = %s.Env()\n", gFilebase, gFilebase)

	fs := token.NewFileSet()
	file, err := parser.ParseFile(fs, gFilename, nil, 0)
	if err != nil {
		panic(err)
	}
	visit := Visitor{}
	ast.Walk(visit, file)
}