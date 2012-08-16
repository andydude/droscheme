package ds_test
import . "ds/any"
import "ds/any/runtime"
var _ds_any_runtime = ds_any_runtime.Export()
var stringZKappend = _ds_any_runtime.Ref("string-append")
var _stringZKappend = stringZKappend.(Named).Value().(func(...Any)Any)
var write = _ds_any_runtime.Ref("write")
var _write = write.(Named).Value().(func(Any, ...Any)Any)
var greet = NewProc(_greet, "greet")
func _greet(name Any) Any {
	return _write(_stringZKappend("Hello ", name))
}
func Export() (env *Env) {
	env = NewEnv()
	env.Add(greet)
	return
}
