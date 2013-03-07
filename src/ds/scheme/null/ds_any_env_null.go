package ds_any_env_null
import . "ds/any"
var nullZKenvironment = NewProc(_nullZKenvironment, "null-environment")
func _nullZKenvironment(version Any) Any {
	env := NewEnv()
	switch version.(int) {
	case 'D':
		//env.Add(applyZKsyntax)
		//env.Add(currentZKenvironment)
		//env.Add(defineZKmacro)
		//env.Add(evalZKsyntax)
		//env.Add(loadZKsyntax)
		fallthrough
	case 7:
		env.Add(defineZKlibrary)
		//env.Add(defineZKrecordZKtype)
		//env.Add(defineZKvalues)
		//env.Add(guard)
		//env.Add(parameterize)
		fallthrough
	case 6:
		//env.Add(assert)
		//env.Add(caseZKlambda)
		//env.Add(identifierZKsyntax)
		env.Add(library)
		//env.Add(quasisyntax)
		//env.Add(syntax)
		//env.Add(syntaxZKcase)
		fallthrough
	case 5:
		//env.Add(defineZKsyntax)
		//env.Add(letZKsyntax)
		//env.Add(letrecZKsyntax)
		//env.Add(syntaxZKrules)
	case 4:
		env.Add(begin)
		env.Add(__case)
		env.Add(cond)
		env.Add(define)
		env.Add(do)
		env.Add(__if)
		env.Add(lambda)
		env.Add(let)
		env.Add(letZH)
		env.Add(letrec)
		env.Add(quasiquote)
		env.Add(quote)
		env.Add(setZA)
		env.Add(unquote)
		env.Add(unquoteZKsplicing)
	}
	return env
}
