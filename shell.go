package droscheme

//func (env AnyDict) Read(source string) Any {
//	return env
//}
//
//func (env AnyDict) Eval(Any) Any {
//	return env
//}
//
//func Print(tree Syntax) {
//}

type Env struct {
	parent *Env
	bound map[string]Any
}

func ShellEnvironment() Env {
	return Env{}
}

func Shell() {
	//var env = ShellEnvironment()
	//Print(env.Eval(env.Read(string)))
}
