package ds_any_syntax
import . "ds/any"
import . "ds/port"
var listZKmatchZS = NewProc(_listZKmatchZS, "list-match?")
func _listZKmatchZS(patt, syntax, env Any) Any {
	__cap, cdp := _carZIcdr(patt)
	if _pairZS(cdp).(bool) {
		cadp := _car(cdp)
		if _symbolZS(cadp).(bool) && (_symbolZKZRstring(cadp) == "...") {
			if !_symbolZS(__cap).(bool) {
				_error("ellipsis is only implemented for symbols")
				return _void()
			}
			if env.(*Env).Ref(__cap) != nil {
				_error("list-match expected unbound symbol")
			}
			if _nullZS(syntax).(bool) || _pairZS(syntax).(bool) {
				return true
			}
			return false
		}
	}
	cas, cds := _carZIcdr(syntax)
	if !_syntaxZKmatchZS(__cap, cas, env).(bool) {
		return false
	}
	if !_syntaxZKmatchZS(cdp, cds, env).(bool) {
		return false
	}
	return true
}
var listZKreplace = NewProc(_listZKreplace, "list-replace")
func _listZKreplace(temp, env Any) Any {
	cat, cdt := _carZIcdr(temp)
	if _pairZS(cdt).(bool) {
		cadt := _car(cdr)
		if _symbolZS(cadt).(bool) && (_symbolZKZRstring(cadt) == "...") {
			return env.(*Env).Ref(cat)
		}
	}
	cas, cds := _syntaxZKreplace(cat, env), _syntaxZKreplace(cdt, env)
	return _cons(cas, cds)
}
var symbolZKmatchZS = NewProc(_symbolZKmatchZS, "symbol-match?")
func _symbolZKmatchZS(patt, syntax, env Any) Any {
	name := _ZKZRimmutableZKstring(patt).(string)
	if name == "_" {
		return true
	}
	if name == "..." {
		_error("we were supposed to catch ... earlier")
	}
	if value := env.(*Env).Ref(patt); (value != nil) && _equalZS(patt, value).(bool) {
		return _equalZS(patt, syntax)
	}
	env.(*Env).Define(patt, syntax)
	return true
}
var symbolZKreplace = NewProc(_symbolZKreplace, "symbol-replace")
func _symbolZKreplace(temp, env Any) Any {
	value := env.(*Env).Ref(temp)
	if value == nil {
		return temp
	}
	return value
}
var syntaxZKmatchZS = NewProc(_syntaxZKmatchZS, "syntax-match?")
func _syntaxZKmatchZS(patt, syntax, env Any) Any {
	if _listZS(patt).(bool) {
		return _listZKmatchZS(patt, syntax, env)
	}
	if _symbolZS(patt).(bool) {
		return _symbolZKmatchZS(patt, syntax, env)
	}
	return _equalZS(patt, syntax)
}
var syntaxZKreplace = NewProc(_syntaxZKreplace, "syntax-replace")
func _syntaxZKreplace(temp, env Any) Any {
	if _listZS(temp).(bool) {
		return _listZKreplace(temp, env)
	}
	if _symbolZS(temp).(bool) {
		return _symbolZKreplace(temp, env)
	}
	return temp
}
var read = NewProc(_read, "read")
func _read(Rest ...Any) Any {
	port := func() Any {
		if len(Rest) > 0 {
			return Rest[0]
		}
		return _currentZKinputZKport()
	}()
	value, err := Read(port.(TIPort))
	if err != nil {
		panic(_ZKZRimmutableZKstring(err))
	}
	return value
}
var write = NewProc(_write, "write")
func _write(obj Any, Rest ...Any) Any {
	port := func() Any {
		if len(Rest) > 0 {
			return Rest[0]
		}
		return _currentZKoutputZKport()
	}()
	rep := _ZKZRschemeZKstring(obj)
	port.(BOPort).Write([]byte(rep.(string)))
	return _void()
}
