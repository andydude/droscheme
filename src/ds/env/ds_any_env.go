package ds_any_env
import . "ds/any"
var defineZKZRlambda = NewProc(_defineZKZRlambda, "define->lambda")
func _defineZKZRlambda(syntax Any) Any {
	return _void()
}
var environment = NewProc(_environment, "environment")
func _environment(importZKspecs ...Any) Any {
	return _error("environment is only available at compile-time")
}
var environmentZKdefine = NewProc(_environmentZKdefine, "environment-define")
func _environmentZKdefine(env Any, symbol Any, value Any) Any {
	name := _ZKZRimmutableZKstring(symbol)
	env.(*Env).Define(name, value)
	return _void()
}
var environmentZKdefineZA = NewProc(_environmentZKdefineZA, "environment-define!")
func _environmentZKdefineZA(env Any, symbol Any, value Any) Any {
	return _void()
}
var environmentZKdefineZKlambda = NewProc(_environmentZKdefineZKlambda, "environment-define-lambda")
func _environmentZKdefineZKlambda(env Any, symbol Any, value Any) Any {
	return _void()
}
var environmentZKextend = NewProc(_environmentZKextend, "environment-extend")
func _environmentZKextend(env Any) Any {
	return env.(*Env).Extend()
}
var environmentZKref = NewProc(_environmentZKref, "environment-ref")
func _environmentZKref(env Any, symbol Any, Rest ...Any) Any {
	value := func() Any {
		if len(Rest) > 0 {
			return Rest[0]
		}
		return __nil
	}()
	name := _ZKZRimmutableZKstring(symbol)
	return env.(*Env).Refer(name, value)
}
var environmentZKsetZA = NewProc(_environmentZKsetZA, "environment-set!")
func _environmentZKsetZA(env Any, symbol Any, value Any) Any {
	name := _ZKZRimmutableZKstring(symbol)
	env.(*Env).Set(name, value)
	return _void()
}
var environmentZKupdate = NewProc(_environmentZKupdate, "environment-update")
func _environmentZKupdate(env Any) Any {
	return env.(*Env).Update()
}
