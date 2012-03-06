package droscheme

func (env AnyDict) Read(source string) Any {
	return env
}

func (env AnyDict) Eval(Any) Any {
	return env
}

func Print(tree Syntax) {
}

func ShellEnvironment() AnyDict {
	return nil
}

func Shell() {
	var env = ShellEnvironment()
	Print(env.Eval(env.Read(string)))
}
