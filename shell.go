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

// (ds builtin)
func BuiltinEnv() *Env {
	return toEnv(DinteractionZKenvironment(list0()))
	//return toEnv(DschemeZKprimitiveZKenvironment(list1(Sint64('D'))))
}

// (eval expr env)
func EvalErr(expr Any, env *Env) (value Any, err error) {
	defer func(){err = panic1error()}()
	return Eval(expr, env), nil
}

func Eval(expr Any, env *Env) Any {
	return Deval(list2(expr, toEnvSpec(env)))
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
	defer func(){err = panic1error()}()
	return Kload(_void(), list1(ToString(filename)), env), nil
}
