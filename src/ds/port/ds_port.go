package ds_port

import (
	"bufio"
	"os"
)

var ()

func Export() map[string]interface{} {
	return map[string]interface{}{"standard-error-port": standardZKerrorZKport, "standard-input-port": standardZKinputZKport, "standard-output-port": standardZKoutputZKport, "open-binary-input-file": openZKbinaryZKinputZKfile, "open-binary-output-file": openZKbinaryZKoutputZKfile, "open-input-file": openZKinputZKfile, "open-output-file": openZKoutputZKfile, "port?": portZS, "binary-port?": binaryZKportZS, "textual-port?": textualZKportZS, "output-port?": outputZKportZS, "input-port?": inputZKportZS}
}

func standardZKerrorZKport() interface{} {
	return gStdErrorPort
}

func standardZKinputZKport() interface{} {
	return gStdInputPort
}

func standardZKoutputZKport() interface{} {
	return gStdOutputPort
}

func openZKbinaryZKinputZKfile(filename interface{}, fileopt interface{}) interface{} {
	var (
		file *os.File
		err  error
	)

	if portZS(fileopt).(bool) {
		file = fileopt.(*os.File)
	}
	if !portZS(fileopt).(bool) {
		file, err = os.Open(filename.(string))
		if err != nil {
			panic(err)
		}
	}
	port := &FilePort{name: filename.(string), kind: KindByteIn}
	port.fl = file
	port.rd = bufio.NewReader(port.fl)
	return port
}

func openZKbinaryZKoutputZKfile(filename interface{}, fileopt interface{}) interface{} {
	var (
		file *os.File
		err  error
	)

	if portZS(fileopt).(bool) {
		file = fileopt.(*os.File)
	}
	if !portZS(fileopt).(bool) {
		file, err = os.Create(filename.(string))
		if err != nil {
			panic(err)
		}
	}
	port := &FilePort{name: filename.(string), kind: KindByteOut}
	port.fl = file
	port.wr = bufio.NewWriter(port.fl)
	return port
}

func openZKbytevectorZKinputZKport() interface{} {
	return false
}

func openZKbytevectorZKoutputZKport() interface{} {
	return false
}

func openZKfileZKinputZKport() interface{} {
	return false
}

func openZKfileZKoutputZKport() interface{} {
	return false
}

func openZKinputZKbytevector() interface{} {
	return false
}

func openZKinputZKfile(filename interface{}, opt ...interface{}) interface{} {
	var (
		file *os.File
		err  error
	)

	switch len(opt) {
	case 0:
		file, err = os.Open(filename.(string))
		if err != nil {
			panic(err)
		}
	case 1:
		file = opt[0].(*os.File)
	}

	port := new(FilePort)
	port.name = filename.(string)
	port.kind = KindCharIn
	port.fl = file
	port.rd = bufio.NewReader(port.fl)
	return port
}

func openZKinputZKstring() interface{} {
	return false
}

func openZKoutputZKbytevector() interface{} {
	return false
}

func openZKoutputZKfile(filename interface{}, opt ...interface{}) interface{} {
	var (
		file *os.File
		err  error
	)

	switch len(opt) {
	case 0:
		file, err = os.Create(filename.(string))
		if err != nil {
			panic(err)
		}
	case 1:
		file = opt[0].(*os.File)
	}

	port := &FilePort{name: filename.(string), kind: KindCharOut}
	port.fl = file
	port.wr = bufio.NewWriter(port.fl)
	return port
}

func openZKoutputZKstring() interface{} {
	return false
}

func openZKstringZKinputZKport() interface{} {
	return false
}

func openZKstringZKoutputZKport() interface{} {
	return false
}

func portZS(a interface{}) interface{} {
	_, ok := a.(PortKinder)
	return ok
}

func binaryZKportZS(a interface{}) interface{} {
	if p, ok := a.(PortKinder); ok {
		switch p.PortKind() {
		case KindByteIn:
			return true
		case KindByteOut:
			return true
		case KindByteInOut:
			return true
		}

	}
	return false
}

func textualZKportZS(a interface{}) interface{} {
	if p, ok := a.(PortKinder); ok {
		switch p.PortKind() {
		case KindCharIn:
			return true
		case KindCharOut:
			return true
		case KindCharInOut:
			return true
		}

	}
	return false
}

func outputZKportZS(a interface{}) interface{} {
	if p, ok := a.(PortKinder); ok {
		switch p.PortKind() {
		case KindByteOut:
			return true
		case KindByteInOut:
			return true
		case KindCharOut:
			return true
		case KindCharInOut:
			return true
		}

	}
	return false
}

func inputZKportZS(a interface{}) interface{} {
	if p, ok := a.(PortKinder); ok {
		switch p.PortKind() {
		case KindByteIn:
			return true
		case KindByteInOut:
			return true
		case KindCharIn:
			return true
		case KindCharInOut:
			return true
		}

	}
	return false
}
