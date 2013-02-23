package ds_port_runtime
import (
.	"ds/any"
	"fmt"
	"os"
)
var standardZKerrorZKport = NewProc(_standardZKerrorZKport, "standard-error-port")
func _standardZKerrorZKport() Any {
	return gStderr
}
var standardZKinputZKport = NewProc(_standardZKinputZKport, "standard-input-port")
func _standardZKinputZKport() Any {
	return gStdin
}
var standardZKoutputZKport = NewProc(_standardZKoutputZKport, "standard-output-port")
func _standardZKoutputZKport() Any {
	return gStdout
}
var openZKbinaryZKinputZKfile = NewProc(_openZKbinaryZKinputZKfile, "open-binary-input-file")
func _openZKbinaryZKinputZKfile(fn Any) Any {
	filename := fn.(fmt.Stringer).String()
	file, err := os.Open(filename)
	error2panic(err)
	return NewFilePort(file, filename, PortTypeCodeByteIn)
}
var openZKbinaryZKoutputZKfile = NewProc(_openZKbinaryZKoutputZKfile, "open-binary-output-file")
func _openZKbinaryZKoutputZKfile(fn Any) Any {
	filename := fn.(fmt.Stringer).String()
	file, err := os.Create(filename)
	error2panic(err)
	return NewFilePort(file, filename, PortTypeCodeByteOut)
}
var openZKbytevectorZKinputZKport = NewProc(_openZKbytevectorZKinputZKport, "open-bytevector-input-port")
func _openZKbytevectorZKinputZKport() Any {
	return _void()
}
var openZKbytevectorZKoutputZKport = NewProc(_openZKbytevectorZKoutputZKport, "open-bytevector-output-port")
func _openZKbytevectorZKoutputZKport() Any {
	return _void()
}
var openZKfileZKinputZKport = NewProc(_openZKfileZKinputZKport, "open-file-input-port")
func _openZKfileZKinputZKport() Any {
	return _void()
}
var openZKfileZKoutputZKport = NewProc(_openZKfileZKoutputZKport, "open-file-output-port")
func _openZKfileZKoutputZKport() Any {
	return _void()
}
var openZKinputZKbytevector = NewProc(_openZKinputZKbytevector, "open-input-bytevector")
func _openZKinputZKbytevector() Any {
	return _void()
}
var openZKinputZKfile = NewProc(_openZKinputZKfile, "open-input-file")
func _openZKinputZKfile(fn Any) Any {
	filename := fn.(fmt.Stringer).String()
	file, err := os.Open(filename)
	error2panic(err)
	return NewFilePort(file, filename, PortTypeCodeCharIn)
}
var openZKinputZKstring = NewProc(_openZKinputZKstring, "open-input-string")
func _openZKinputZKstring() Any {
	return _void()
}
var openZKoutputZKbytevector = NewProc(_openZKoutputZKbytevector, "open-output-bytevector")
func _openZKoutputZKbytevector() Any {
	return _void()
}
var openZKoutputZKfile = NewProc(_openZKoutputZKfile, "open-output-file")
func _openZKoutputZKfile(fn Any) Any {
	filename := fn.(fmt.Stringer).String()
	file, err := os.Create(filename)
	error2panic(err)
	return NewFilePort(file, filename, PortTypeCodeCharOut)
}
var openZKoutputZKstring = NewProc(_openZKoutputZKstring, "open-output-string")
func _openZKoutputZKstring() Any {
	return _void()
}
var openZKstringZKinputZKport = NewProc(_openZKstringZKinputZKport, "open-string-input-port")
func _openZKstringZKinputZKport() Any {
	return _void()
}
var openZKstringZKoutputZKport = NewProc(_openZKstringZKoutputZKport, "open-string-output-port")
func _openZKstringZKoutputZKport() Any {
	return _void()
}
