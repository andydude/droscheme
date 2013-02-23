package ds_num_runtime
import . "ds/any"
import "ds/any/runtime"
var _ds_any_runtime = ds_any_runtime.Export()
var void = _ds_any_runtime.Ref("void").(Named)
var _void = void.Value().(func(o ...Any)Any)
var voidZS = _ds_any_runtime.Ref("void?").(Named)
var _voidZS = voidZS.Value().(func(a Any)Any)
