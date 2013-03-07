package main

import (
	"ds/port"
//	"ds/scheme/eval"
//	"ds/scheme/parameter"
	"ds/scheme/read"
	"ds/scheme/write"
)

var _ds_port = ds_port.Export()
var standardZKinputZKport = _ds_port["standard-input-port"].(func()interface{})
var standardZKoutputZKport = _ds_port["standard-output-port"].(func()interface{})

//var _ds_scheme_eval = ds_scheme_eval.Export()
//var eval = _ds_scheme_eval["eval"]
//var _eval = eval.(func(interface{},interface{})interface{})

var _ds_scheme_read = ds_scheme_read.Export()
var readZKlines = _ds_scheme_read["read-lines"].(func(interface{})interface{})
var read = _ds_scheme_read["read"].(func(interface{})interface{})

var _ds_scheme_write = ds_scheme_write.Export()
var write = _ds_scheme_write["write"].(func(interface{},interface{})interface{})

//var _ds_scheme_parameter = ds_scheme_parameter.Export()
//var interactionZKenvironment = _ds_scheme_parameter["interaction-environment"]
//var _interactionZKenvironment = interactionZKenvironment.(func(...interface{})interface{})
