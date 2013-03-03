package ds_any_env
import . "ds/any"
func Export()(env *Env) {
	env = NewEnv()
	env.Add(defineZKZRlambda)
	env.Add(environment)
	env.Add(environmentZKdefine)
	env.Add(environmentZKdefineZA)
	env.Add(environmentZKdefineZKlambda)
	env.Add(environmentZKextend)
	env.Add(environmentZKref)
	env.Add(environmentZKsetZA)
	env.Add(environmentZKupdate)
	return
}
