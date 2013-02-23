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

type ErrorObject struct {
    msg string
    it Any
    code int
}

func (eo ErrorObject) GetType() int {
    return TypeCodeError
}

func (eo ErrorObject) GetHash() uintptr {
    return _stringZKhash(eo.Error()).(uintptr)
}

func (eo ErrorObject) GetErrorObjectType() int {
    return eo.code
}

func (eo ErrorObject) Irritants() Any {
    return eo.it
}

func (eo ErrorObject) Error() string {
    return eo.msg
}
