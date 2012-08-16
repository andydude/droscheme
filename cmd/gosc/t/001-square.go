package ds_test
import . "ds/any"
import "ds/any/runtime"
var _ds_any_runtime = ds_any_runtime.Export()
var __ZH = _ds_any_runtime.Ref("*")
var _ZH = __ZH.(*Proc).Call().(func(...Any)Any)
var square = NewProc(_square, "square")
func _square(x Any) Any {
	return _ZH(x, x)
}
func Export() (env *Env) {
	env = NewEnv()
	env.Add(square)
	return
}
