package ds_port

import (
	"testing"
)

func TestInput(t *testing.T) {
	var err error
	input := openZKinputZKfile("t/example.txt").(*FilePort)

	// read ' '
	r, size, err := input.ReadRune()
	if err != nil {
		t.Errorf("an error happened while reading: %s", err.Error())
	}
	if size != 1 {
		t.Errorf("read multiple runes: %d", size)
	}
	if r != ' ' {
		t.Errorf("read non space: %s", string([]rune{r}))
	}

	//// read '.'
	//r, size, err = input.ReadRune()
	//if err != nil {
	//	t.Errorf("an error happened while reading: %s", err.Error())
	//}
	//if size != 1 {
	//	t.Errorf("read multiple runes: %d", size)
	//}
	//if r != '.' {
	//	t.Errorf("read non space: %s", string([]rune{r}))
	//}
	//
	//input.UnreadRune()
	input.UnreadRune()

	// read ' '
	r, size, err = input.ReadRune()
	if err != nil {
		t.Errorf("an error happened while reading: %s", err.Error())
	}
	if size != 1 {
		t.Errorf("read multiple runes: %d", size)
	}
	if r != ' ' {
		t.Errorf("read non space: %s", string([]rune{r}))
	}

	// close
	err = input.Close()
	if err != nil {
		t.Errorf("an error happened while closing: %s", err.Error())
	}
}

func TestOutput(t *testing.T) {
	var err error
	output := openZKoutputZKfile("t/output.txt").(*FilePort)

	_, err = output.WriteString("hello world")
	if err != nil {
		t.Errorf("an error happened while writing: %s", err.Error())
	}

	err = output.Flush()
	if err != nil {
		t.Errorf("an error happened while flusing: %s", err.Error())
	}

	err = output.Close()
	if err != nil {
		t.Errorf("an error happened while closing: %s", err.Error())
	}
}
