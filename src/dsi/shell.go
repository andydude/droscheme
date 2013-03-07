//
// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins, Daniel Connelly
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
//
package main

import (
.	"ds/any"
.	"ds/port"
	"strings"
)

func CountParens(s string) int {
    return strings.Count(s, "(") - strings.Count(s, ")")
}

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

func SetCmdLine(args []string) {
    aargs := []Any{}

    for _, str := range args {
        aargs = append(aargs, ToString(str))
        if str == "-f" {
            aargs = []Any{}
        }
    }

	//_commandZKline(Vector(aargs).ToList())
}

//func _commandZKlineZKsetZA(args []string) {
//    gCmdLine.(Applier).Apply(list1(NewVector(aargs).ToList()))
//}

func GetRootPath() string {
    return os.Getenv("DROSCHEME_ROOT")
}

// (ds base)
func BuiltinEnv() *Env {
	return _interactionZKenvironment()
}

// (eval expr env)
func EvalErr(expr Any, env *Env) (value Any, err error) {
	//defer func(){err = panic1error()}()
	return Eval(expr, env), nil
}

func Eval(expr Any, env *Env) Any {
	return _eval(expr, env)
}

func ReadFile(filename string) (Any, error) {
	port := OpenTIFile(filename).(TIPort)
	return Read(port)
}

func ReadString(input string) (Any, error) {
	port := OpenTIPort(input).(TIPort)
	return Read(port)
}


func ReadState(port TIPort, state State) (value Any, err error) {
	lex := newLexerWithState(port, state)
	return ReadLexer(lex)
}

func ReadLexer(lex *Lexer) (value Any, err error) {
	yyParse(lex)
	if lex.pcount == 0 {
		err = lex.err
	} else {
		err = gEOL
	} 
	value = lex.value
	return
}

func Read(port TIPort) (value Any, err error) {
	return ReadState(port, (*Lexer).lexToken)
}

func Load(filename string, env *Env) (value Any, err error) {
	//defer func(){err = panic1error()}()
	return Kload(_void(), list1(ToString(filename)), env), nil
}
