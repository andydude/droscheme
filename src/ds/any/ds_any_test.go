package ds_any

import (
	"reflect"
	"testing"
)

func TestValuesRoundTrip(t *testing.T) {
	vl := values("hello", "world")
	vc := valuesZKZRvector(vl).([]interface{})
	vl2 := vectorZKZRvalues(vc)
	if gk := vl2[0].Kind(); gk != reflect.String {
		t.Errorf("string type not maintained")
	}
	if gk := vl2[1].Kind(); gk != reflect.String {
		t.Errorf("string type not maintained")
	}
	if vl2[0].String() != "hello" {
		t.Errorf("string not maintained")
	}
	if vl2[1].String() != "world" {
		t.Errorf("string not maintained")
	}
}

func TestVectorRoundTrip(t *testing.T) {
	vc := vector("hello", "world")
	vl := vectorZKZRvalues(vc)
	vc2 := valuesZKZRvector(vl).([]interface{})
	if _, ok := vc2[0].(string); !ok {
		t.Errorf("string type not maintained")
	}
	if _, ok := vc2[1].(string); !ok {
		t.Errorf("string type not maintained")
	}
	if vc2[0].(string) != "hello" {
		t.Errorf("string not maintained")
	}
	if vc2[1].(string) != "world" {
		t.Errorf("string not maintained")
	}
}
