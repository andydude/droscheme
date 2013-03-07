package ds_scheme_read

import (
	"ds/any"
	"fmt"
)

type Label struct {
	it    interface{}
	label int
}

func NewLabel(l, d interface{}) interface{} {
	return Label{it: d, label: int(l.(Label).it.(int))}
}

func (o Label) Hash() uintptr {
	return 0
}

func (o Label) Kind() int {
	return ds_any.KindLabel
}

func (o Label) Equal(a interface{}) bool {
	return false
}

func (o Label) String() string {
	if o.it == nil {
		return fmt.Sprintf("#%d#", o.label)
	}
	return fmt.Sprintf("#%d=%s", o.label, o.it)
}
