package ds_num_s64

import ()

var ()

func Export() map[string]interface{} {
	return map[string]interface{}{"+": ZI, "-": ZK, "*": ZH, "+/carry": ZIZMcarry, "-/carry": ZKZMcarry, "*/carry": ZHZMcarry, "=": ZQ, "<": ZP, "<=": ZPZQ, ">": ZR, ">=": ZRZQ, "abs": abs, "bit-and": bitZKand, "bit-count": bitZKcount, "bit-eqv": bitZKeqv, "bit-field": bitZKfield, "bit-if": bitZKif, "bit-implies": bitZKimplies, "bit-nif": bitZKnif, "bit-nimplies": bitZKnimplies, "bit-not": bitZKnot, "bit-or": bitZKor, "bit-set?": bitZKsetZS, "bit-xor": bitZKxor, "complex?": complexZS, "copy-bit": copyZKbit, "copy-bit-field": copyZKbitZKfield, "euc/": eucZM, "euc%": eucZE, "even?": evenZS, "exact?": exactZS, "first-bit-set": firstZKbitZKset, "greatest": greatest, "inexact?": inexactZS, "integer?": integerZS, "least": least, "length": length, "max": max, "min": min, "negative?": negativeZS, "number?": numberZS, "odd?": oddZS, "positive?": positiveZS, "rational?": rationalZS, "real?": realZS, "reverse-bit-field": reverseZKbitZKfield, "rotate-bit-field": rotateZKbitZKfield, "rtz/": rtzZM, "rtz%": rtzZE, "rtn/": rtnZM, "rtn%": rtnZE, "rtp/": rtpZM, "rtp%": rtpZE, "rta/": rtaZM, "rta%": rtaZE, "rnz/": rnzZM, "rnz%": rnzZE, "rnn/": rnnZM, "rnn%": rnnZE, "rnp/": rnpZM, "rnp%": rnpZE, "rna/": rnaZM, "rna%": rnaZE, "rte/": rteZM, "rte%": rteZE, "shift": shift, "shift-left": shiftZKleft, "shift-right": shiftZKright, "width": width, "zero?": zeroZS}
}

func ZI(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		return 0
	case 1:
		return rest[0].(int64)
	case 2:
		return (rest[0].(int64) + rest[1].(int64))
	}

	rv := rest[0].(int64)
	for i := 1; i < len(rest); i++ {
		it := rest[i].(int64)
		rv += it
	}
	return rv
}

func ZK(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		return (-rest[0].(int64))
	case 2:
		return (rest[0].(int64) - rest[1].(int64))
	}

	return ZK(rest[0], ZI(rest[1:]...))
}

func ZH(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		return 1
	case 1:
		return rest[0]
	case 2:
		return (rest[0].(int64) * rest[1].(int64))
	}

	rv := rest[0].(int64)
	for i := 1; i < len(rest); i++ {
		it := rest[i].(int64)
		rv *= it
	}
	return rv
}

func ZIZMcarry(a interface{}, b interface{}, c interface{}) interface{} {
	return false
}

func ZKZMcarry(a interface{}, b interface{}, c interface{}) interface{} {
	return false
}

func ZHZMcarry(a interface{}, b interface{}, c interface{}) interface{} {
	return false
}

func ZQ(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (rest[0].(int64) == rest[1].(int64))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(rest[i].(int64) == rest[(i+1)].(int64)) {
			return false
		}
	}
	return true
}

func ZP(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (rest[0].(int64) < rest[1].(int64))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(rest[i].(int64) < rest[(i+1)].(int64)) {
			return false
		}
	}
	return true
}

func ZPZQ(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (rest[0].(int64) <= rest[1].(int64))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(rest[i].(int64) <= rest[(i+1)].(int64)) {
			return false
		}
	}
	return true
}

func ZR(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (rest[0].(int64) > rest[1].(int64))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(rest[i].(int64) > rest[(i+1)].(int64)) {
			return false
		}
	}
	return true
}

func ZRZQ(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (rest[0].(int64) >= rest[1].(int64))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		a := rest[i].(int64)
		b := rest[(i + 1)].(int64)
		if !(a >= b) {
			return false
		}
	}
	return true
}

func abs(a interface{}) interface{} {
	if a.(int64) < 0 {
		return ZK(a)
	}
	return a
}

func bitZKand(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		return int64(-1)
	case 1:
		return rest[0].(int64)
	}

	rv := rest[0].(int64)
	for i := 1; i < len(rest); i++ {
		it := rest[i].(int64)
		rv &= it
	}
	return rv
}

func bitZKcount() interface{} {
	return false
}

func bitZKeqv(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		return int64(-1)
	case 1:
		return rest[0].(int64)
	}

	rv := rest[0].(int64)
	for i := 1; i < len(rest); i++ {
		it := rest[i].(int64)
		rv = (^(rv ^ it))
	}
	return rv
}

func bitZKfield() interface{} {
	return false
}

func bitZKif(a interface{}, b interface{}) interface{} {
	return bitZKnot(bitZKnimplies(b, a))
}

func bitZKimplies(a interface{}, b interface{}) interface{} {
	return bitZKnot(bitZKnimplies(a, b))
}

func bitZKnif(a interface{}, b interface{}) interface{} {
	return bitZKnimplies(b, a)
}

func bitZKnimplies(a interface{}, b interface{}) interface{} {
	return (a.(int64) &^ b.(int64))
}

func bitZKnot(a interface{}) interface{} {
	return (^a.(int64))
}

func bitZKor(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		return int64(-1)
	case 1:
		return rest[0].(int64)
	}

	rv := rest[0].(int64)
	for i := 1; i < len(rest); i++ {
		it := rest[i].(int64)
		rv |= it
	}
	return rv
}

func bitZKsetZS() interface{} {
	return false
}

func bitZKxor(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		return int64(-1)
	case 1:
		return rest[0].(int64)
	}

	rv := rest[0].(int64)
	for i := 1; i < len(rest); i++ {
		it := rest[i].(int64)
		rv ^= it
	}
	return rv
}

func complexZS(a interface{}) interface{} {
	return true
}

func copyZKbit() interface{} {
	return false
}

func copyZKbitZKfield() interface{} {
	return false
}

func eucZM(a interface{}, b interface{}) interface{} {
	return rtzZM(ZK(a, eucZE(a, b)), b)
}

func eucZE(a interface{}, b interface{}) interface{} {
	switch {
	case negativeZS(rtzZE(a, b)).(bool):
		return ZI(rtzZE(a, b), abs(b))
	}

	return rtzZE(a, b)
}

func evenZS(a interface{}) interface{} {
	return ((a.(int64) % 2) == 0)
}

func exactZS(a interface{}) interface{} {
	return true
}

func firstZKbitZKset() interface{} {
	return false
}

func greatest() interface{} {
	return int64(9223372036854775807)
}

func inexactZS(a interface{}) interface{} {
	return false
}

func integerZS(a interface{}) interface{} {
	return true
}

func least() interface{} {
	return int64(-9223372036854775808)
}

func length(a interface{}) interface{} {
	return func() interface{} {
		if negativeZS(a).(bool) {
			return length(ZK(a))
		}
		return true
	}()
}

func max(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		return rest[0].(int64)
	}

	rv := rest[0].(int64)
	for i := 1; i < len(rest); i++ {
		it := rest[i].(int64)
		if it > rv {
			rv = it
		}
	}
	return rv
}

func min(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		return rest[0].(int64)
	}

	rv := rest[0].(int64)
	for i := 1; i < len(rest); i++ {
		it := rest[i].(int64)
		if it < rv {
			rv = it
		}
	}
	return rv
}

func negativeZS(a interface{}) interface{} {
	return (a.(int64) < 0)
}

func numberZS(a interface{}) interface{} {
	return true
}

func oddZS(a interface{}) interface{} {
	return ((a.(int64) % 2) != 0)
}

func positiveZS(a interface{}) interface{} {
	return (a.(int64) > 0)
}

func rationalZS(a interface{}) interface{} {
	return true
}

func realZS(a interface{}) interface{} {
	return true
}

func reverseZKbitZKfield() interface{} {
	return false
}

func rotateZKbitZKfield() interface{} {
	return false
}

func rtzZM(a interface{}, b interface{}) interface{} {
	return (a.(int64) / b.(int64))
}

func rtzZE(a interface{}, b interface{}) interface{} {
	return (a.(int64) % b.(int64))
}

func rtnZM(a interface{}, b interface{}) interface{} {
	return rtzZM(ZK(a, rtnZE(a, b)), b)
}

func rtnZE(a interface{}, b interface{}) interface{} {
	switch {
	case (rtzZE(a, b).(int64) == 0):
		break
	case negativeZS(ZH(a, b)).(bool):
		return (rtzZE(a, b).(int64) + b.(int64))
	}

	return rtzZE(a, b)
}

func rtpZM(a interface{}, b interface{}) interface{} {
	return rtzZM(ZK(a, rtpZE(a, b)), b)
}

func rtpZE(a interface{}, b interface{}) interface{} {
	return rtnZE(a, ZK(b))
}

func rtaZM(a interface{}, b interface{}) interface{} {
	return rtzZM(ZK(a, rtaZE(a, b)), b)
}

func rtaZE(a interface{}, b interface{}) interface{} {
	switch {
	case (rtzZE(a, b).(int64) == 0):
		break
	case positiveZS(ZH(a, b)).(bool):
		return (rtzZE(a, b).(int64) - b.(int64))
	case negativeZS(ZH(a, b)).(bool):
		return (rtzZE(a, b).(int64) + b.(int64))
	}

	return rtzZE(a, b)
}

func rnzZM(a interface{}, b interface{}) interface{} {
	return rtzZM(ZK(a, rnzZE(a, b)), b)
}

func rnzZE(a interface{}, b interface{}) interface{} {
	var two int64 = 2

	switch {
	case zeroZS(rtzZE(a, b)).(bool):
		break
	case ZQ(rtzZE(ZH(two, a), ZH(two, b)), b).(bool):
		break
	case ZQ(rtaZE(ZH(two, a), ZH(two, b)), b).(bool):
		break
	case ZR(rtnZE(ZH(two, a), ZH(two, b)), b).(bool):
		if positiveZS(a).(bool) {
			switch {
			case positiveZS(b).(bool):
				return ZK(rtzZE(a, b), b)
			default:
				return ZI(rtzZE(a, b), b)
			}

		}
		break
	case ZP(rtnZE(ZH(two, a), ZH(two, b)), b).(bool):
		if negativeZS(a).(bool) {
			switch {
			case negativeZS(b).(bool):
				return ZK(rtzZE(a, b), b)
			default:
				return ZI(rtzZE(a, b), b)
			}

		}
		break
	}

	return rtzZE(a, b)
}

func rnnZM(a interface{}, b interface{}) interface{} {
	var two int64 = 2

	switch {
	case zeroZS(rtzZE(a, b)).(bool):
		break
	case ZQ(rtzZE(ZH(two, a), ZH(two, b)), b).(bool):
		return rtnZM(a, b)
	case ZQ(rtaZE(ZH(two, a), ZH(two, b)), b).(bool):
		return rtnZM(a, b)
	}

	return rnzZM(a, b)
}

func rnnZE(a interface{}, b interface{}) interface{} {
	return ZK(a, ZH(b, rnnZM(a, b)))
}

func rnpZM(a interface{}, b interface{}) interface{} {
	var two int64 = 2

	switch {
	case zeroZS(rtzZE(a, b)).(bool):
		break
	case ZQ(rtzZE(ZH(two, a), ZH(two, b)), b).(bool):
		return rtpZM(a, b)
	case ZQ(rtaZE(ZH(two, a), ZH(two, b)), b).(bool):
		return rtpZM(a, b)
	}

	return rnzZM(a, b)
}

func rnpZE(a interface{}, b interface{}) interface{} {
	return ZK(a, ZH(b, rnpZM(a, b)))
}

func rnaZM(a interface{}, b interface{}) interface{} {
	var two int64 = 2

	switch {
	case zeroZS(rtzZE(a, b)).(bool):
		break
	case ZQ(rtzZE(ZH(two, a), ZH(two, b)), b).(bool):
		return rtaZM(a, b)
	case ZQ(rtaZE(ZH(two, a), ZH(two, b)), b).(bool):
		return rtaZM(a, b)
	}

	return rnzZM(a, b)
}

func rnaZE(a interface{}, b interface{}) interface{} {
	return ZK(a, ZH(b, rnaZM(a, b)))
}

func rteZM(a interface{}, b interface{}) interface{} {
	var forceZKrteZM func(a interface{}, b interface{}) interface{}
	forceZKrteZM = func(a interface{}, b interface{}) interface{} {
		return ZH(int64(2), rtnZM(ZI(rnnZM(a, b), int64(1)), int64(2)))
	}
	var two int64 = 2

	switch {
	case zeroZS(rtzZE(a, b)).(bool):
		break
	case ZQ(rtzZE(ZH(two, a), ZH(two, b)), b).(bool):
		return forceZKrteZM(a, b)
	case ZQ(rtaZE(ZH(two, a), ZH(two, b)), b).(bool):
		return forceZKrteZM(a, b)
	}

	return rnzZM(a, b)
}

func rteZE(a interface{}, b interface{}) interface{} {
	return (a.(int64) % b.(int64))
}

func shift() interface{} {
	return false
}

func shiftZKleft() interface{} {
	return false
}

func shiftZKright() interface{} {
	return false
}

func width() interface{} {
	return int64(64)
}

func zeroZS(a interface{}) interface{} {
	return (a.(int64) == 0)
}
