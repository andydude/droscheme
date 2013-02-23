package ds_any_syntax
import (
.	"ds/any"
)
func Export() (env *Env) {
	env = NewEnv()
	//env.Add(__begin)
	//env.Add(__if)
	env.Add(read)
	env.Add(write)
	return
}
