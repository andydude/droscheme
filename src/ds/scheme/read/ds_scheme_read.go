package ds_scheme_read

import (
	"ds/any"
	"ds/any/error"
	"ds/port"
	"ds/scheme/parameter"
)

var (
	_ds_any                    = (ds_any.Export)()
	__append                   = _ds_any["append"].(func(a interface{}, rest ...interface{}) interface{})
	apply                      = _ds_any["apply"].(func(proc interface{}, args ...interface{}) interface{})
	applyZKlist                = _ds_any["apply-list"].(func(proc interface{}, args interface{}) interface{})
	applyZKvector              = _ds_any["apply-vector"].(func(proc interface{}, args interface{}) interface{})
	primitiveZKapplyZKvector   = _ds_any["primitive-apply-vector"].(func(proc interface{}, args interface{}) interface{})
	booleanZKand               = _ds_any["boolean-and"].(func(rest ...interface{}) interface{})
	booleanZKor                = _ds_any["boolean-or"].(func(rest ...interface{}) interface{})
	booleanZQZS                = _ds_any["boolean=?"].(func(a interface{}, b interface{}) interface{})
	booleanZS                  = _ds_any["boolean?"].(func(a interface{}) interface{})
	boolean                    = _ds_any["boolean"].(func(a interface{}) interface{})
	bytevectorZKZRu8ZKlist     = _ds_any["bytevector->u8-list"].(func(a interface{}) interface{})
	bytevectorZKZRu8ZKvector   = _ds_any["bytevector->u8-vector"].(func(a interface{}) interface{})
	bytevectorZS               = _ds_any["bytevector?"].(func(a interface{}) interface{})
	call                       = _ds_any["call"].(func(proc interface{}, rest ...interface{}) interface{})
	car                        = _ds_any["car"].(func(a interface{}) interface{})
	cdr                        = _ds_any["cdr"].(func(a interface{}) interface{})
	cons                       = _ds_any["cons"].(func(a interface{}, b interface{}) interface{})
	eofZKobject                = _ds_any["eof-object"].(func() interface{})
	eofZKobjectZS              = _ds_any["eof-object?"].(func(a interface{}) interface{})
	equalZS                    = _ds_any["equal?"].(func(a interface{}, b interface{}) interface{})
	appendZA                   = _ds_any["append!"].(func(rest ...interface{}) interface{})
	cdrs                       = _ds_any["cdrs"].(func(lists interface{}) interface{})
	cars                       = _ds_any["cars"].(func(lists interface{}) interface{})
	carsZI                     = _ds_any["cars+"].(func(lists interface{}, lastZKelt interface{}) interface{})
	foldZKright                = _ds_any["fold-right"].(func(kons interface{}, knil interface{}, lis1 interface{}, lists ...interface{}) interface{})
	immZKlistZS                = _ds_any["imm-list?"].(func(a interface{}) interface{})
	immZKnull                  = _ds_any["imm-null"].(func(o ...interface{}) interface{})
	immZKnullZS                = _ds_any["imm-null?"].(func(a interface{}) interface{})
	immZKpairZS                = _ds_any["imm-pair?"].(func(a interface{}) interface{})
	immZKstarZS                = _ds_any["imm-star?"].(func(a interface{}) interface{})
	immZKstarZKlength          = _ds_any["imm-star-length"].(func(a interface{}) interface{})
	length                     = _ds_any["length"].(func(ls interface{}) interface{})
	list                       = _ds_any["list"].(func(o ...interface{}) interface{})
	listZH                     = _ds_any["list*"].(func(o ...interface{}) interface{})
	listZI                     = _ds_any["list+"].(func(first interface{}, rest ...interface{}) interface{})
	listZKZRstring             = _ds_any["list->string"].(func(a interface{}) interface{})
	listZKZRvector             = _ds_any["list->vector"].(func(ls interface{}) interface{})
	listZS                     = _ds_any["list?"].(func(a interface{}) interface{})
	map1                       = _ds_any["map1"].(func(proc interface{}, ls interface{}) interface{})
	__map                      = _ds_any["map"].(func(proc interface{}, rest ...interface{}) interface{})
	any1                       = _ds_any["any1"].(func(pred interface{}, ls interface{}) interface{})
	any                        = _ds_any["any"].(func(pred interface{}, rest ...interface{}) interface{})
	every1                     = _ds_any["every1"].(func(pred interface{}, ls interface{}) interface{})
	every                      = _ds_any["every"].(func(pred interface{}, rest ...interface{}) interface{})
	mZKnullZS                  = _ds_any["m-null?"].(func(a interface{}) interface{})
	mZKpairZS                  = _ds_any["m-pair?"].(func(a interface{}) interface{})
	mZKstarZS                  = _ds_any["m-star?"].(func(a interface{}) interface{})
	mZKstringZS                = _ds_any["m-string?"].(func(a interface{}) interface{})
	makeZKbytevector           = _ds_any["make-bytevector"].(func(k interface{}) interface{})
	makeZKenvironment          = _ds_any["make-environment"].(func() interface{})
	makeZKstring               = _ds_any["make-string"].(func(k interface{}) interface{})
	makeZKvector               = _ds_any["make-vector"].(func(k interface{}) interface{})
	not                        = _ds_any["not"].(func(a interface{}) interface{})
	null                       = _ds_any["null"].(func(o ...interface{}) interface{})
	nullZS                     = _ds_any["null?"].(func(a interface{}) interface{})
	objectZKequalZS            = _ds_any["object-equal?"].(func(a interface{}, b interface{}) interface{})
	pairZS                     = _ds_any["pair?"].(func(a interface{}) interface{})
	s8ZS                       = _ds_any["s8?"].(func(a interface{}) interface{})
	s16ZS                      = _ds_any["s16?"].(func(a interface{}) interface{})
	s32ZS                      = _ds_any["s32?"].(func(a interface{}) interface{})
	s64ZS                      = _ds_any["s64?"].(func(a interface{}) interface{})
	stringZKappend             = _ds_any["string-append"].(func(rest ...interface{}) interface{})
	immZKstringZKZRstring      = _ds_any["imm-string->string"].(func(str interface{}) interface{})
	stringZKZRimmZKstring      = _ds_any["string->imm-string"].(func(str interface{}) interface{})
	symbolZKZRstring           = _ds_any["symbol->string"].(func(a interface{}) interface{})
	symbolZQZS                 = _ds_any["symbol=?"].(func(a interface{}, b interface{}) interface{})
	symbolZS                   = _ds_any["symbol?"].(func(a interface{}) interface{})
	typeZKcheck                = _ds_any["type-check"].(func(expected interface{}, irrs ...interface{}) interface{})
	typeZKerror                = _ds_any["type-error"].(func(expected interface{}, irr interface{}) interface{})
	u8ZKlistZKZRbytevector     = _ds_any["u8-list->bytevector"].(func(a interface{}) interface{})
	u8ZKvectorZKZRbytevector   = _ds_any["u8-vector->bytevector"].(func(a interface{}) interface{})
	u8ZS                       = _ds_any["u8?"].(func(a interface{}) interface{})
	u16ZS                      = _ds_any["u16?"].(func(a interface{}) interface{})
	u32ZS                      = _ds_any["u32?"].(func(a interface{}) interface{})
	u64ZS                      = _ds_any["u64?"].(func(a interface{}) interface{})
	vector                     = _ds_any["vector"].(func(o ...interface{}) interface{})
	vectorZKZRlist             = _ds_any["vector->list"].(func(a interface{}) interface{})
	vectorZKZRstring           = _ds_any["vector->string"].(func(a interface{}) interface{})
	vectorZKlength             = _ds_any["vector-length"].(func(vc interface{}) interface{})
	vectorZKmap                = _ds_any["vector-map"].(func(proc interface{}, vc interface{}) interface{})
	vectorZS                   = _ds_any["vector?"].(func(a interface{}) interface{})
	void                       = _ds_any["void"].(func() interface{})
	voidZS                     = _ds_any["void?"].(func(a interface{}) interface{})
	ZKZRimmZKstring            = _ds_any["->imm-string"].(func(str interface{}) interface{})
	ZKZRschemeZKstring         = _ds_any["->scheme-string"].(func(a interface{}) interface{})
	ZKZRstring                 = _ds_any["->string"].(func(str interface{}) interface{})
	_ds_any_error              = (ds_any_error.Export)()
	makeZKerrorZKobject        = _ds_any_error["make-error-object"].(func(msg interface{}, irrs ...interface{}) interface{})
	errorZKobjectZS            = _ds_any_error["error-object?"].(func(obj interface{}) interface{})
	errorZKobjectZKmessage     = _ds_any_error["error-object-message"].(func(obj interface{}) interface{})
	errorZKobjectZKirritants   = _ds_any_error["error-object-irritants"].(func(obj interface{}) interface{})
	__error                    = _ds_any_error["error"].(func(msg interface{}, irrs ...interface{}) interface{})
	raise                      = _ds_any_error["raise"].(func(obj interface{}) interface{})
	raiseZKcontinuable         = _ds_any_error["raise-continuable"].(func(obj interface{}) interface{})
	_ds_port                   = (ds_port.Export)()
	standardZKerrorZKport      = _ds_port["standard-error-port"].(func() interface{})
	standardZKinputZKport      = _ds_port["standard-input-port"].(func() interface{})
	standardZKoutputZKport     = _ds_port["standard-output-port"].(func() interface{})
	openZKbinaryZKinputZKfile  = _ds_port["open-binary-input-file"].(func(filename interface{}, fileopt interface{}) interface{})
	openZKbinaryZKoutputZKfile = _ds_port["open-binary-output-file"].(func(filename interface{}, fileopt interface{}) interface{})
	openZKinputZKfile          = _ds_port["open-input-file"].(func(filename interface{}, opt ...interface{}) interface{})
	openZKoutputZKfile         = _ds_port["open-output-file"].(func(filename interface{}, opt ...interface{}) interface{})
	portZS                     = _ds_port["port?"].(func(a interface{}) interface{})
	binaryZKportZS             = _ds_port["binary-port?"].(func(a interface{}) interface{})
	textualZKportZS            = _ds_port["textual-port?"].(func(a interface{}) interface{})
	outputZKportZS             = _ds_port["output-port?"].(func(a interface{}) interface{})
	inputZKportZS              = _ds_port["input-port?"].(func(a interface{}) interface{})
	_ds_scheme_parameter       = (ds_scheme_parameter.Export)()
	commandZKline              = _ds_scheme_parameter["command-line"].(func(rest ...interface{}) interface{})
	currentZKerrorZKport       = _ds_scheme_parameter["current-error-port"].(func(rest ...interface{}) interface{})
	currentZKinputZKport       = _ds_scheme_parameter["current-input-port"].(func(rest ...interface{}) interface{})
	currentZKoutputZKport      = _ds_scheme_parameter["current-output-port"].(func(rest ...interface{}) interface{})
	makeZKparameter            = _ds_scheme_parameter["make-parameter"].(func(init interface{}, conv interface{}, name interface{}) interface{})
	parameterZKconverter       = _ds_scheme_parameter["parameter-converter"].(func(param interface{}) interface{})
	parameterZKname            = _ds_scheme_parameter["parameter-name"].(func(param interface{}) interface{})
	parameterZKref             = _ds_scheme_parameter["parameter-ref"].(func(param interface{}) interface{})
	parameterZKsetZA           = _ds_scheme_parameter["parameter-set!"].(func(param interface{}, value interface{}) interface{})
)

func Export() map[string]interface{} {
	return map[string]interface{}{"read-data": readZKdata, "read-lines": readZKlines, "read": read}
}

func readZKlexer(lex *Lexer) (value interface{}, err error) {
	yyParse(lex)
	if lex.pcount == 0 {
		err = lex.err
	} else {
		err = gEOL
	}

	value = lex.value
	return
}

func readZKstate(state State, port interface{}) interface{} {
	value, err := readZKlexer(newLexerWithState(port.(ds_port.TIPort), state))
	if err != nil {
		panic(err)
	}
	return value
}

func readZKdata(port interface{}) interface{} {
	return read(port)
}

func readZKwithZKprompt(prompt interface{}, port interface{}, outputZKport interface{}) interface{} {
	return read(port)
}

func readZKexpression(port interface{}, prompt interface{}) interface{} {
	return null()
}

func readZKlines(port interface{}) interface{} {
	return readZKwithZKprompt("    ", port, currentZKoutputZKport())
}

func read(port interface{}) interface{} {
	value, err := readZKlexer(newLexer(port.(ds_port.TIPort)))
	if err != nil {
		panic(err)
	}
	return value
}
