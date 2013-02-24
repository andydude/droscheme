package ds_any_runtime

import "testing"

func TestIsList(t *testing.T) {
	ls := _cons(42, _null())
	if _, ok := ls.(*Pair); !ok {
		t.Fail()
	}
	if !_pairZS(ls).(bool) {
		t.Fail()
	}
}

func TestMakeList(t *testing.T) {
	ls := _makeZKlist(2).(*Pair)
	ls.Set(0, 11)
	ls.Set(1, 12)
	if ls.Ref(0).(int) != 11 {
		t.Fail()
	}
	if ls.Ref(1).(int) != 12 {
		t.Fail()
	}
}

func TestListLength(t *testing.T) {
}
