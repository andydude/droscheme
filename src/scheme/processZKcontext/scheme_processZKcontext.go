package scheme_processZKcontext
import . "ds/any"
import (
	"ds/base"
	"os"
)
var commandZKlineZKvector = NewProc(_commandZKlineZKvector, "command-line-vector")
func _commandZKlineZKvector() Any {
	return Vector(os.Args)
}
var commandZKline = NewProc(_commandZKline, "command-line")
func _commandZKline() Any {
	return _vectorZKZRlist(_commandZKlineZKvector())
}
var exit = NewProc(_exit, "exit")
func _exit(Rest ...Any) Any {
	obj := func() Any {
		if len(Rest) > 0 {
			return Rest[0]
		}
		return true
	}()
	code := func() Any {
		if obj {
			return 0
		}
		return 1
	}()
	os.Exit(code)
	return _void()
}
var getZKenvironmentZKvariable = NewProc(_getZKenvironmentZKvariable, "get-environment-variable")
func _getZKenvironmentZKvariable(name Any) Any {
	return func() Any {
		var key = _stringZKZRimmutableZKstring(name)
		return _immutableZKstringZKZRstring(os.Getenv(key))
	}()
}
var getZKenvironmentZKvariablesZKvector = NewProc(_getZKenvironmentZKvariablesZKvector, "get-environment-variables-vector")
func _getZKenvironmentZKvariablesZKvector() Any {
	env := os.Environ()
	ret := _makeZKvector(len(env)).(Vector)
	for k, pair := range env {
		str := _immutableZKstringZKZRstring(pair)
		idx := _stringZKindex(str, '=')
		key := _substring(str, 0, idx)
		val := _substring(str, idx+1)
		ret.Set(k, _cons(key, val))
	}
	return ret
}
var getZKenvironmentZKvariables = NewProc(_getZKenvironmentZKvariables, "get-environment-variables")
func _getZKenvironmentZKvariables() Any {
	return _vectorZKZRlist(_getZKenvironmentZKvariablesZKvector())
}
