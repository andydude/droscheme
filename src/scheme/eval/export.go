package scheme_eval
import . "ds/any"
func Export()(env *Env) {
	env = NewEnv()
	env.Add(environment)
	env.Add(eval)
	env.Add(nullZKenvironment)
	env.Add(schemeZKreportZKenvironment)
	return
}
