package ds_any_syntax

import (
.	"ds/any"
	"ds/any/runtime"
	"fmt"
)

type Label struct {
	it    Any
	label int
}

func NewLabel(l, d Any) Any {
    return Label{it: d, label: int(l.(Label).it.(int))}
}

func (o Label) GetHash() uintptr {
    return 0
}

func (o Label) GetType() int {
    return ds_any_runtime.TypeCodeLabel
}

func (o Label) Equal(a Any) bool {
    return false
}

func (o Label) String() string {
    if o.it == nil {
        return fmt.Sprintf("#%d#", o.label)
    }
    return fmt.Sprintf("#%d=%s", o.label, o.it)
}
