package ds_any_syntax

import . "ds/any"

//func NewSyntax(a, b Any) Any {
//	//return &Syntax{a, b}
//	return nil
//}

//type GoStringer interface {
//	GoString() string
//}
//
//type SchemeStringer interface {
//	SchemeString() string
//}

// Matcher

// This interface represents the first phase
// of (syntax-rules) transformations. It is
// similar to Equal, except that it takes an
// environment, which is normally empty, and
// is used as an extra return value in which
// pattern variables are bound.
//
// The pattern variables are used by the next
// phase, so in order to save energy later, we
// assign any pattern variables found in the
// object that implements this method and then
// store them in env. The same environment is
// also used for literals, which must be given
// to the Match method in the form x=x.
//
type Matcher interface {
	Match(syntax Any, env *Env) bool
}

// Replacer

// This interface represents the second phase
// of (syntax-rules) transformations. It is
// similar to Eval, except in the way that the
// environment is used. Instead of raising an
// error every time a symbol is unbound, it
// continues silently, and only replaces those
// symbols which are in the environment.
//
// The pattern variables are given in env by the
// Match method, in such a way that we can treat
// literals the same as pattern variables, since
// they are of the form x=x (see above). Hence,
// the literals can safely be replaced just like
// the pattern variables. This is the magic
// I was hoping to describe in this comment.
//
type Replacer interface {
	Replace(env *Env) Any
}
