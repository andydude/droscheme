package ds_base
import . "ds/any"
func Export()(env *Env) {
	env = NewEnv()
	env.Add(listZKZRvector)
	env.Add(vectorZKZRlist)
	return
}
