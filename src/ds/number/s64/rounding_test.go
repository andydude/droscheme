package ds_num_s64

import (
	"testing"
)

func assertEq(t *testing.T, a, b int64) {
	if a != b {
		t.Errorf("expected %d, but got %d.", a, b)
	}
}

func assertRound(t *testing.T, f func(a, b interface{}) interface{}) {

	// tests an integer point
	assertEq(t, 2, f(int64(6), int64(3)).(int64))
	assertEq(t, 2, f(int64(-6), int64(-3)).(int64))
	assertEq(t, -2, f(int64(-6), int64(3)).(int64))
	assertEq(t, -2, f(int64(6), int64(-3)).(int64))

	// tests an integer point
	assertEq(t, 3, f(int64(6), int64(2)).(int64))
	assertEq(t, 3, f(int64(-6), int64(-2)).(int64))
	assertEq(t, -3, f(int64(-6), int64(2)).(int64))
	assertEq(t, -3, f(int64(6), int64(-2)).(int64))
}

func assertRoundToNearest(t *testing.T, f func(a, b interface{}) interface{}) {

	// tests whether we round away zero
	assertEq(t, 2, f(int64(7), int64(4)).(int64))
	assertEq(t, 2, f(int64(-7), int64(-4)).(int64))
	assertEq(t, -2, f(int64(-7), int64(4)).(int64))
	assertEq(t, -2, f(int64(7), int64(-4)).(int64))

	// tests whether we round towards zero
	assertEq(t, 2, f(int64(9), int64(4)).(int64))
	assertEq(t, 2, f(int64(-9), int64(-4)).(int64))
	assertEq(t, -2, f(int64(-9), int64(4)).(int64))
	assertEq(t, -2, f(int64(9), int64(-4)).(int64))
}

func TestRtzDiv_1(t *testing.T) {
	assertRound(t, rtzZM)

	assertEq(t, 3, rtzZM(int64(7), int64(2)).(int64))
	assertEq(t, 3, rtzZM(int64(-7), int64(-2)).(int64))
	assertEq(t, -3, rtzZM(int64(-7), int64(2)).(int64))
	assertEq(t, -3, rtzZM(int64(7), int64(-2)).(int64))
}

func TestRtnDiv_1(t *testing.T) {
	assertRound(t, rtnZM)

	assertEq(t, 3, rtnZM(int64(7), int64(2)).(int64))
	assertEq(t, 3, rtnZM(int64(-7), int64(-2)).(int64))
	assertEq(t, -4, rtnZM(int64(-7), int64(2)).(int64))
	assertEq(t, -4, rtnZM(int64(7), int64(-2)).(int64))
}

func TestRtaDiv_1(t *testing.T) {
	assertRound(t, rtaZM)

	assertEq(t, 4, rtaZM(int64(7), int64(2)).(int64))
	assertEq(t, 4, rtaZM(int64(-7), int64(-2)).(int64))
	assertEq(t, -4, rtaZM(int64(-7), int64(2)).(int64))
	assertEq(t, -4, rtaZM(int64(7), int64(-2)).(int64))
}

func TestRtpDiv_1(t *testing.T) {
	assertRound(t, rtpZM)

	assertEq(t, 4, rtpZM(int64(7), int64(2)).(int64))
	assertEq(t, 4, rtpZM(int64(-7), int64(-2)).(int64))
	assertEq(t, -3, rtpZM(int64(-7), int64(2)).(int64))
	assertEq(t, -3, rtpZM(int64(7), int64(-2)).(int64))
}

func TestRnzDiv_1(t *testing.T) {
	assertRoundToNearest(t, rnzZM)

	assertEq(t, 3, rnzZM(int64(7), int64(2)).(int64))
	assertEq(t, 3, rnzZM(int64(-7), int64(-2)).(int64))
	assertEq(t, -3, rnzZM(int64(-7), int64(2)).(int64))
	assertEq(t, -3, rnzZM(int64(7), int64(-2)).(int64))
}

func TestRnnDiv_1(t *testing.T) {
	assertRoundToNearest(t, rnnZM)

	assertEq(t, 3, rnnZM(int64(7), int64(2)).(int64))
	assertEq(t, 3, rnnZM(int64(-7), int64(-2)).(int64))
	assertEq(t, -4, rnnZM(int64(-7), int64(2)).(int64))
	assertEq(t, -4, rnnZM(int64(7), int64(-2)).(int64))
}

func TestRnaDiv_1(t *testing.T) {
	assertRoundToNearest(t, rnaZM)

	assertEq(t, 4, rnaZM(int64(7), int64(2)).(int64))
	assertEq(t, 4, rnaZM(int64(-7), int64(-2)).(int64))
	assertEq(t, -4, rnaZM(int64(-7), int64(2)).(int64))
	assertEq(t, -4, rnaZM(int64(7), int64(-2)).(int64))
}

func TestRnpDiv_1(t *testing.T) {
	assertRoundToNearest(t, rnpZM)

	assertEq(t, 4, rnpZM(int64(7), int64(2)).(int64))
	assertEq(t, 4, rnpZM(int64(-7), int64(-2)).(int64))
	assertEq(t, -3, rnpZM(int64(-7), int64(2)).(int64))
	assertEq(t, -3, rnpZM(int64(7), int64(-2)).(int64))
}

func TestRteDiv_1(t *testing.T) {
	assertRoundToNearest(t, rteZM)

	assertEq(t, 4, rteZM(int64(7), int64(2)).(int64))
	assertEq(t, 4, rteZM(int64(-7), int64(-2)).(int64))
	assertEq(t, -4, rteZM(int64(-7), int64(2)).(int64))
	assertEq(t, -4, rteZM(int64(7), int64(-2)).(int64))

	assertEq(t, 4, rteZM(int64(9), int64(2)).(int64))
	assertEq(t, 4, rteZM(int64(-9), int64(-2)).(int64))
	assertEq(t, -4, rteZM(int64(-9), int64(2)).(int64))
	assertEq(t, -4, rteZM(int64(9), int64(-2)).(int64))

	assertEq(t, 6, rteZM(int64(11), int64(2)).(int64))
	assertEq(t, 6, rteZM(int64(-11), int64(-2)).(int64))
	assertEq(t, -6, rteZM(int64(-11), int64(2)).(int64))
	assertEq(t, -6, rteZM(int64(11), int64(-2)).(int64))
}

func TestEucDiv_1(t *testing.T) {
	assertEq(t, 2, eucZM(int64(6), int64(3)).(int64))
	assertEq(t, 2, eucZM(int64(-6), int64(-3)).(int64))
	assertEq(t, -2, eucZM(int64(-6), int64(3)).(int64))
	assertEq(t, -2, eucZM(int64(6), int64(-3)).(int64))

	assertEq(t, 3, eucZM(int64(6), int64(2)).(int64))
	assertEq(t, 3, eucZM(int64(-6), int64(-2)).(int64))
	assertEq(t, -3, eucZM(int64(-6), int64(2)).(int64))
	assertEq(t, -3, eucZM(int64(6), int64(-2)).(int64))

	assertEq(t, 3, eucZM(int64(7), int64(2)).(int64))
	assertEq(t, 4, eucZM(int64(-7), int64(-2)).(int64))
	assertEq(t, -4, eucZM(int64(-7), int64(2)).(int64))
	assertEq(t, -3, eucZM(int64(7), int64(-2)).(int64))
}
