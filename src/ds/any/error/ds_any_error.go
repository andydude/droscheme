package ds_any_error

import ()

var ()

func Export() map[string]interface{} {
	return map[string]interface{}{"make-error-object": makeZKerrorZKobject, "error-object?": errorZKobjectZS, "error-object-message": errorZKobjectZKmessage, "error-object-irritants": errorZKobjectZKirritants, "error": __error, "raise": raise, "raise-continuable": raiseZKcontinuable}
}

func makeZKerrorZKobject(msg interface{}, irrs ...interface{}) interface{} {
	return ErrorObject{message: msg.(string), irritants: irrs}
}

func errorZKobjectZS(obj interface{}) interface{} {
	_, ok := obj.(ErrorObject)
	return ok
}

func errorZKobjectZKmessage(obj interface{}) interface{} {
	return (obj.(ErrorObject).Message)()
}

func errorZKobjectZKirritants(obj interface{}) interface{} {
	return (obj.(ErrorObject).Irritants)()
}

func __error(msg interface{}, irrs ...interface{}) interface{} {
	return raise(makeZKerrorZKobject(msg, irrs...))
}

func raise(obj interface{}) interface{} {
	panic(obj)
	return nil
}

func raiseZKcontinuable(obj interface{}) interface{} {
	panic(obj)
	return nil
}
