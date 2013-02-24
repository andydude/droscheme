// Droscheme - a Scheme implementation
// Copyright © 2012 Andrew Robbins
//
// This program is free software: it is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.

package ds_any_runtime

import (
.	"ds/any"
)

func NewParameter(value Any, name string) *PrimProc {
	return _makeZKparameter(value, values, name).(*PrimProc)
}

var (
	// (define command-line (make-parameter (vector)))
	commandZKline = NewParameter(_vector(), "command-line")
	_commandZKline = commandZKline.Value().(func(...Any) Any)
	currentZKerrorZKport = NewParameter(_void(), "current-error-port")
	_currentZKerrorZKport = currentZKerrorZKport.Value().(func(...Any) Any)
	currentZKinputZKport = NewParameter(_void(), "current-input-port")
	_currentZKinputZKport = currentZKinputZKport.Value().(func(...Any) Any)
	currentZKoutputZKport = NewParameter(_void(), "current-output-port")
	_currentZKoutputZKport = currentZKoutputZKport.Value().(func(...Any) Any)
)

// (make-parameter init)
// (make-parameter init converter)
// (make-parameter init converter name)
var makeZKparameter = NewProc(_makeZKparameter, "make-parameter")
func _makeZKparameter(init Any, Rest ...Any) Any {
	conv := func() Any {
		if len(Rest) > 0 {
			return Rest[0]
		}
		return nil
	}()

	name := func() Any {
		if len(Rest) > 1 {
			return Rest[1]
		}
		return "(make-parameter)"
	}()

    pc := _pointerZKof(conv).(uintptr)
    if gParameters == nil {
        gParameters = make(map[uintptr]Any, 8)
    }

	// fast case
	var param = func(rest ...Any) Any {
		cell := gParameters[pc]
		switch len(rest) {
		case 0:
			return cell
		case 1:
			gParameters[pc] = rest[0]
			return _void()
		}
		// reserved for future use
		return nil
	}

	// OPT
	if conv != nil {
		// slow case
		param = func(rest ...Any) Any {
			cell := gParameters[pc]
			switch len(rest) {
			case 0:
				return cell
			case 1:
				gParameters[pc] = _apply(conv, rest[0])
				return _void()
			}
			// reserved for future use
			return nil
		}
	}

	param(init)
    return NewProc(param, name.(string))
}

//var parameterize = NewSyntax(_parameterize, "parameterize")
//func _parameterize(Rest ...Any) Any {
//}