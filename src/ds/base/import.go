package ds_base
import . "ds/any"
import (
	"ds/any/char"
	"ds/any/env"
	"ds/any/runtime"
	"ds/any/syntax"
	"ds/port/runtime"
)
var charZKZRinteger = _ds_any_char.Ref("char->integer").(Named)
var _charZKZRinteger = charZKZRinteger.Value().(func()(ch Any))
var charZS = _ds_any_char.Ref("char?").(Named)
var _charZS = charZS.Value().(func()(a Any))
var charZKalphabeticZS = _ds_any_char.Ref("char-alphabetic?").(Named)
var _charZKalphabeticZS = charZKalphabeticZS.Value().(func()(ch Any))
var charZKnumericZS = _ds_any_char.Ref("char-numeric?").(Named)
var _charZKnumericZS = charZKnumericZS.Value().(func()(ch Any))
var digitZKvalue = _ds_any_char.Ref("digit-value").(Named)
var _digitZKvalue = digitZKvalue.Value().(func()(ch Any))
var integerZKZRchar = _ds_any_char.Ref("integer->char").(Named)
var _integerZKZRchar = integerZKZRchar.Value().(func()(cp Any))
var defineZKZRlambda = _ds_any_env.Ref("define->lambda").(Named)
var _defineZKZRlambda = defineZKZRlambda.Value().(func()(syntax Any))
var environment = _ds_any_env.Ref("environment").(Named)
var _environment = environment.Value().(func()(importZKspecs ...Any))
var environmentZKdefine = _ds_any_env.Ref("environment-define").(Named)
var _environmentZKdefine = environmentZKdefine.Value().(func()(env, symbol, value Any))
var environmentZKextend = _ds_any_env.Ref("environment-extend").(Named)
var _environmentZKextend = environmentZKextend.Value().(func()(env Any))
var environmentZKref = _ds_any_env.Ref("environment-ref").(Named)
var _environmentZKref = environmentZKref.Value().(func(env, symbol Any)(Rest ...Any))
var environmentZKsetZA = _ds_any_env.Ref("environment-set!").(Named)
var _environmentZKsetZA = environmentZKsetZA.Value().(func()(env, symbol, value Any))
var apply = _ds_any_runtime.Ref("apply").(Named)
var _apply = apply.Value().(func(proc Any)(args ...Any))
var booleanZS = _ds_any_runtime.Ref("boolean?").(Named)
var _booleanZS = booleanZS.Value().(func()(a Any))
var booleanZQZS = _ds_any_runtime.Ref("boolean=?").(Named)
var _booleanZQZS = booleanZQZS.Value().(func()(a, b Any))
var bytevector = _ds_any_runtime.Ref("bytevector").(Named)
var _bytevector = bytevector.Value().(func()(rest ...Any))
var bytevectorZKZRu8ZKlist = _ds_any_runtime.Ref("bytevector->u8-list").(Named)
var _bytevectorZKZRu8ZKlist = bytevectorZKZRu8ZKlist.Value().(func()(a Any))
var bytevectorZKZRu8ZKvector = _ds_any_runtime.Ref("bytevector->u8-vector").(Named)
var _bytevectorZKZRu8ZKvector = bytevectorZKZRu8ZKvector.Value().(func()(a Any))
var bytevectorZS = _ds_any_runtime.Ref("bytevector?").(Named)
var _bytevectorZS = bytevectorZS.Value().(func()(a Any))
var car = _ds_any_runtime.Ref("car").(Named)
var _car = car.Value().(func()(ls Any))
var carZIcdr = _ds_any_runtime.Ref("car+cdr").(Named)
var _carZIcdr = carZIcdr.Value().(func(ls Any)(car, cdr Any))
var cdr = _ds_any_runtime.Ref("cdr").(Named)
var _cdr = cdr.Value().(func()(ls Any))
var charZKZRinteger = _ds_any_runtime.Ref("char->integer").(Named)
var _charZKZRinteger = charZKZRinteger.Value().(func()(ch Any))
var charZQZS = _ds_any_runtime.Ref("char=?").(Named)
var _charZQZS = charZQZS.Value().(func()(a, b Any))
var charZS = _ds_any_runtime.Ref("char?").(Named)
var _charZS = charZS.Value().(func()(a Any))
var cons = _ds_any_runtime.Ref("cons").(Named)
var _cons = cons.Value().(func()(a, b Any))
var eofZKobject = _ds_any_runtime.Ref("eof-object").(Named)
var _eofZKobject = eofZKobject.Value().(func())
var eofZKobjectZS = _ds_any_runtime.Ref("eof-object?").(Named)
var _eofZKobjectZS = eofZKobjectZS.Value().(func()(a Any))
var eqZS = _ds_any_runtime.Ref("eq?").(Named)
var _eqZS = eqZS.Value().(func()(a, b Any))
var objectZKequalZS = _ds_any_runtime.Ref("object-equal?").(Named)
var _objectZKequalZS = objectZKequalZS.Value().(func()(a, b Any))
var equalZS = _ds_any_runtime.Ref("equal?").(Named)
var _equalZS = equalZS.Value().(func()(a, b Any))
var eqvZS = _ds_any_runtime.Ref("eqv?").(Named)
var _eqvZS = eqvZS.Value().(func()(a, b Any))
var error = _ds_any_runtime.Ref("error").(Named)
var _error = error.Value().(func(msg Any)(irr ...Any))
var errorZKobject = _ds_any_runtime.Ref("error-object").(Named)
var _errorZKobject = errorZKobject.Value().(func(msg Any)(irr ...Any))
var errorZKobjectZKirritants = _ds_any_runtime.Ref("error-object-irritants").(Named)
var _errorZKobjectZKirritants = errorZKobjectZKirritants.Value().(func()(a Any))
var errorZKobjectZKmessage = _ds_any_runtime.Ref("error-object-message").(Named)
var _errorZKobjectZKmessage = errorZKobjectZKmessage.Value().(func()(a Any))
var errorZKobjectZS = _ds_any_runtime.Ref("error-object?").(Named)
var _errorZKobjectZS = errorZKobjectZS.Value().(func()(a Any))
var exactZQZS = _ds_any_runtime.Ref("exact=?").(Named)
var _exactZQZS = exactZQZS.Value().(func()(a, b Any))
var exactZS = _ds_any_runtime.Ref("exact?").(Named)
var _exactZS = exactZS.Value().(func()(a Any))
var inexactZQZS = _ds_any_runtime.Ref("inexact=?").(Named)
var _inexactZQZS = inexactZQZS.Value().(func()(a, b Any))
var inexactZS = _ds_any_runtime.Ref("inexact?").(Named)
var _inexactZS = inexactZS.Value().(func()(a Any))
var integerZKZRchar = _ds_any_runtime.Ref("integer->char").(Named)
var _integerZKZRchar = integerZKZRchar.Value().(func()(cp Any))
var lastZKpair = _ds_any_runtime.Ref("last-pair").(Named)
var _lastZKpair = lastZKpair.Value().(func()(ls Any))
var length = _ds_any_runtime.Ref("length").(Named)
var _length = length.Value().(func()(ls Any))
var listZH = _ds_any_runtime.Ref("list*").(Named)
var _listZH = listZH.Value().(func()(o ...Any))
var listZI = _ds_any_runtime.Ref("list+").(Named)
var _listZI = listZI.Value().(func(a Any)(rest ...Any))
var listZS = _ds_any_runtime.Ref("list?").(Named)
var _listZS = listZS.Value().(func()(a Any))
var list = _ds_any_runtime.Ref("list").(Named)
var _list = list.Value().(func()(o ...Any))
var listZHZKZRvector = _ds_any_runtime.Ref("list*->vector").(Named)
var _listZHZKZRvector = listZHZKZRvector.Value().(func()(pr Any))
var listZKZRstring = _ds_any_runtime.Ref("list->string").(Named)
var _listZKZRstring = listZKZRstring.Value().(func()(a Any))
var listZKZRvector = _ds_any_runtime.Ref("list->vector").(Named)
var _listZKZRvector = listZKZRvector.Value().(func()(ls Any))
var makeZKbytevector = _ds_any_runtime.Ref("make-bytevector").(Named)
var _makeZKbytevector = makeZKbytevector.Value().(func()(k Any))
var makeZKlist = _ds_any_runtime.Ref("make-list").(Named)
var _makeZKlist = makeZKlist.Value().(func()(k Any))
var makeZKstring = _ds_any_runtime.Ref("make-string").(Named)
var _makeZKstring = makeZKstring.Value().(func()(k Any))
var makeZKvector = _ds_any_runtime.Ref("make-vector").(Named)
var _makeZKvector = makeZKvector.Value().(func()(k Any))
var not = _ds_any_runtime.Ref("not").(Named)
var _not = not.Value().(func()(bl Any))
var null = _ds_any_runtime.Ref("null").(Named)
var _null = null.Value().(func()(o ...Any))
var nullZS = _ds_any_runtime.Ref("null?").(Named)
var _nullZS = nullZS.Value().(func()(a Any))
var pairZS = _ds_any_runtime.Ref("pair?").(Named)
var _pairZS = pairZS.Value().(func()(a Any))
var pointerZKof = _ds_any_runtime.Ref("pointer-of").(Named)
var _pointerZKof = pointerZKof.Value().(func()(a Any))
var pointerZQZS = _ds_any_runtime.Ref("pointer=?").(Named)
var _pointerZQZS = pointerZQZS.Value().(func()(a, b Any))
var procedureZS = _ds_any_runtime.Ref("procedure?").(Named)
var _procedureZS = procedureZS.Value().(func()(a Any))
var raise = _ds_any_runtime.Ref("raise").(Named)
var _raise = raise.Value().(func()(err Any))
var string = _ds_any_runtime.Ref("string").(Named)
var _string = string.Value().(func()(chars ...Any))
var stringZKhash = _ds_any_runtime.Ref("string-hash").(Named)
var _stringZKhash = stringZKhash.Value().(func()(a Any))
var stringZKZRlist = _ds_any_runtime.Ref("string->list").(Named)
var _stringZKZRlist = stringZKZRlist.Value().(func()(a Any))
var stringZKZRvector = _ds_any_runtime.Ref("string->vector").(Named)
var _stringZKZRvector = stringZKZRvector.Value().(func()(a Any))
var stringZS = _ds_any_runtime.Ref("string?").(Named)
var _stringZS = stringZS.Value().(func()(a Any))
var symbolZQZS = _ds_any_runtime.Ref("symbol=?").(Named)
var _symbolZQZS = symbolZQZS.Value().(func()(a, b Any))
var stringZKZRsymbol = _ds_any_runtime.Ref("string->symbol").(Named)
var _stringZKZRsymbol = stringZKZRsymbol.Value().(func()(a Any))
var symbolZKZRstring = _ds_any_runtime.Ref("symbol->string").(Named)
var _symbolZKZRstring = symbolZKZRstring.Value().(func()(a Any))
var symbolZS = _ds_any_runtime.Ref("symbol?").(Named)
var _symbolZS = symbolZS.Value().(func()(a Any))
var u8ZKlistZKZRbytevector = _ds_any_runtime.Ref("u8-list->bytevector").(Named)
var _u8ZKlistZKZRbytevector = u8ZKlistZKZRbytevector.Value().(func()(a Any))
var u8ZKvectorZKZRbytevector = _ds_any_runtime.Ref("u8-vector->bytevector").(Named)
var _u8ZKvectorZKZRbytevector = u8ZKvectorZKZRbytevector.Value().(func()(a Any))
var vectorZKZRlistZH = _ds_any_runtime.Ref("vector->list*").(Named)
var _vectorZKZRlistZH = vectorZKZRlistZH.Value().(func()(a Any))
var vectorZKZRstring = _ds_any_runtime.Ref("vector->string").(Named)
var _vectorZKZRstring = vectorZKZRstring.Value().(func()(a Any))
var vector = _ds_any_runtime.Ref("vector").(Named)
var _vector = vector.Value().(func()(o ...Any))
var vectorZKZRlist = _ds_any_runtime.Ref("vector->list").(Named)
var _vectorZKZRlist = vectorZKZRlist.Value().(func()(a Any))
var vectorZS = _ds_any_runtime.Ref("vector?").(Named)
var _vectorZS = vectorZS.Value().(func()(a Any))
var void = _ds_any_runtime.Ref("void").(Named)
var _void = void.Value().(func()(o ...Any))
var voidZS = _ds_any_runtime.Ref("void?").(Named)
var _voidZS = voidZS.Value().(func()(a Any))
var u8ZS = _ds_any_runtime.Ref("u8?").(Named)
var _u8ZS = u8ZS.Value().(func()(a Any))
var typeZQZS = _ds_any_runtime.Ref("type=?").(Named)
var _typeZQZS = typeZQZS.Value().(func()(a, b Any))
var stringZKZRimmutableZKstring = _ds_any_runtime.Ref("string->immutable-string").(Named)
var _stringZKZRimmutableZKstring = stringZKZRimmutableZKstring.Value().(func()(a Any))
var immutableZKstringZKZRstring = _ds_any_runtime.Ref("immutable-string->string").(Named)
var _immutableZKstringZKZRstring = immutableZKstringZKZRstring.Value().(func()(a Any))
var ZKZRgoZKstring = _ds_any_runtime.Ref("->go-string").(Named)
var _ZKZRgoZKstring = ZKZRgoZKstring.Value().(func()(a Any))
var ZKZRimmutableZKstring = _ds_any_runtime.Ref("->immutable-string").(Named)
var _ZKZRimmutableZKstring = ZKZRimmutableZKstring.Value().(func()(a Any))
var ZKZRschemeZKstring = _ds_any_runtime.Ref("->scheme-string").(Named)
var _ZKZRschemeZKstring = ZKZRschemeZKstring.Value().(func()(a Any))
var ZKZRstring = _ds_any_runtime.Ref("->string").(Named)
var _ZKZRstring = ZKZRstring.Value().(func()(a Any))
var listZKmatchZS = _ds_any_syntax.Ref("list-match?").(Named)
var _listZKmatchZS = listZKmatchZS.Value().(func()(patt, syntax, env Any))
var listZKreplace = _ds_any_syntax.Ref("list-replace").(Named)
var _listZKreplace = listZKreplace.Value().(func()(temp, env Any))
var symbolZKmatchZS = _ds_any_syntax.Ref("symbol-match?").(Named)
var _symbolZKmatchZS = symbolZKmatchZS.Value().(func()(patt, syntax, env Any))
var symbolZKreplace = _ds_any_syntax.Ref("symbol-replace").(Named)
var _symbolZKreplace = symbolZKreplace.Value().(func()(temp, env Any))
var syntaxZKmatchZS = _ds_any_syntax.Ref("syntax-match?").(Named)
var _syntaxZKmatchZS = syntaxZKmatchZS.Value().(func()(patt, syntax, env Any))
var syntaxZKreplace = _ds_any_syntax.Ref("syntax-replace").(Named)
var _syntaxZKreplace = syntaxZKreplace.Value().(func()(temp, env Any))
var read = _ds_any_syntax.Ref("read").(Named)
var _read = read.Value().(func()(Rest ...Any))
var write = _ds_any_syntax.Ref("write").(Named)
var _write = write.Value().(func(obj Any)(Rest ...Any))
var standardZKerrorZKport = _ds_port_runtime.Ref("standard-error-port").(Named)
var _standardZKerrorZKport = standardZKerrorZKport.Value().(func())
var standardZKinputZKport = _ds_port_runtime.Ref("standard-input-port").(Named)
var _standardZKinputZKport = standardZKinputZKport.Value().(func())
var standardZKoutputZKport = _ds_port_runtime.Ref("standard-output-port").(Named)
var _standardZKoutputZKport = standardZKoutputZKport.Value().(func())
var openZKbinaryZKinputZKfile = _ds_port_runtime.Ref("open-binary-input-file").(Named)
var _openZKbinaryZKinputZKfile = openZKbinaryZKinputZKfile.Value().(func()(filename Any))
var openZKbinaryZKoutputZKfile = _ds_port_runtime.Ref("open-binary-output-file").(Named)
var _openZKbinaryZKoutputZKfile = openZKbinaryZKoutputZKfile.Value().(func()(filename Any))
var openZKbytevectorZKinputZKport = _ds_port_runtime.Ref("open-bytevector-input-port").(Named)
var _openZKbytevectorZKinputZKport = openZKbytevectorZKinputZKport.Value().(func())
var openZKbytevectorZKoutputZKport = _ds_port_runtime.Ref("open-bytevector-output-port").(Named)
var _openZKbytevectorZKoutputZKport = openZKbytevectorZKoutputZKport.Value().(func())
var openZKfileZKinputZKport = _ds_port_runtime.Ref("open-file-input-port").(Named)
var _openZKfileZKinputZKport = openZKfileZKinputZKport.Value().(func())
var openZKfileZKoutputZKport = _ds_port_runtime.Ref("open-file-output-port").(Named)
var _openZKfileZKoutputZKport = openZKfileZKoutputZKport.Value().(func())
var openZKinputZKbytevector = _ds_port_runtime.Ref("open-input-bytevector").(Named)
var _openZKinputZKbytevector = openZKinputZKbytevector.Value().(func())
var openZKinputZKfile = _ds_port_runtime.Ref("open-input-file").(Named)
var _openZKinputZKfile = openZKinputZKfile.Value().(func()(filename Any))
var openZKinputZKstring = _ds_port_runtime.Ref("open-input-string").(Named)
var _openZKinputZKstring = openZKinputZKstring.Value().(func())
var openZKoutputZKbytevector = _ds_port_runtime.Ref("open-output-bytevector").(Named)
var _openZKoutputZKbytevector = openZKoutputZKbytevector.Value().(func())
var openZKoutputZKfile = _ds_port_runtime.Ref("open-output-file").(Named)
var _openZKoutputZKfile = openZKoutputZKfile.Value().(func()(filename Any))
var openZKoutputZKstring = _ds_port_runtime.Ref("open-output-string").(Named)
var _openZKoutputZKstring = openZKoutputZKstring.Value().(func())
var openZKstringZKinputZKport = _ds_port_runtime.Ref("open-string-input-port").(Named)
var _openZKstringZKinputZKport = openZKstringZKinputZKport.Value().(func())
var openZKstringZKoutputZKport = _ds_port_runtime.Ref("open-string-output-port").(Named)
var _openZKstringZKoutputZKport = openZKstringZKoutputZKport.Value().(func())