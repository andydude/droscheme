package ds_any_char

import "unicode"

var ()

func Export() map[string]interface{} {
	return map[string]interface{}{"char->integer": charZKZRinteger, "char-alphabetic?": charZKalphabeticZS, "char-bin-digit?": charZKbinZKdigitZS, "char-ci<=?": charZKciZPZQZS, "char-ci<?": charZKciZPZS, "char-ci=?": charZKciZQZS, "char-ci>=?": charZKciZRZQZS, "char-ci>?": charZKciZRZS, "char-digit?": charZKdigitZS, "char-downcase": charZKdowncase, "char-foldcase": charZKfoldcase, "char-hex-digit?": charZKhexZKdigitZS, "char-lower-case?": charZKlowerZKcaseZS, "char-numeric?": charZKnumericZS, "char-octal-digit?": charZKoctalZKdigitZS, "char-punctuation?": charZKpunctuationZS, "char-symbolic?": charZKsymbolicZS, "char-title-case?": charZKtitleZKcaseZS, "char-titlecase": charZKtitlecase, "char-upcase": charZKupcase, "char-upper-case?": charZKupperZKcaseZS, "char-whitespace?": charZKwhitespaceZS, "char<=?": charZPZQZS, "char<?": charZPZS, "char=?": charZQZS, "char>=?": charZRZQZS, "char>?": charZRZS, "digit-value": digitZKvalue, "hex-digit-value": hexZKdigitZKvalue, "integer->char": integerZKZRchar}
}

func charZKZRinteger(ch interface{}) interface{} {
	return int64(ch.(rune))
}

func charZKalphabeticZS(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.IsLetter(cp)
}

func charZKbinZKdigitZS(ch interface{}) interface{} {
	cp := ch.(rune)
	return ((48 == cp) || (cp == 49))
}

func charZKciZPZQZS(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (charZKfoldcase(rest[0]).(rune) <= charZKfoldcase(rest[1]).(rune))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(charZKfoldcase(rest[i]).(rune) <= charZKfoldcase(rest[(i+1)]).(rune)) {
			return false
		}
	}
	return true
}

func charZKciZPZS(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (charZKfoldcase(rest[0]).(rune) < charZKfoldcase(rest[1]).(rune))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(charZKfoldcase(rest[i]).(rune) < charZKfoldcase(rest[(i+1)]).(rune)) {
			return false
		}
	}
	return true
}

func charZKciZQZS(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (charZKfoldcase(rest[0]).(rune) == charZKfoldcase(rest[1]).(rune))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(charZKfoldcase(rest[i]).(rune) == charZKfoldcase(rest[(i+1)]).(rune)) {
			return false
		}
	}
	return true
}

func charZKciZRZQZS(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (charZKfoldcase(rest[0]).(rune) >= charZKfoldcase(rest[1]).(rune))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(charZKfoldcase(rest[i]).(rune) >= charZKfoldcase(rest[(i+1)]).(rune)) {
			return false
		}
	}
	return true
}

func charZKciZRZS(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (charZKfoldcase(rest[0]).(rune) > charZKfoldcase(rest[1]).(rune))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(charZKfoldcase(rest[i]).(rune) > charZKfoldcase(rest[(i+1)]).(rune)) {
			return false
		}
	}
	return true
}

func charZKdigitZS(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.IsDigit(cp)
}

func charZKdowncase(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.ToLower(cp)
}

func charZKfoldcase(ch interface{}) interface{} {
	return charZKdowncase(charZKupcase(ch))
}

func charZKhexZKdigitZS(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.IsOneOf([]*unicode.RangeTable{unicode.Hex_Digit}, cp)
}

func charZKlowerZKcaseZS(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.IsLower(cp)
}

func charZKnumericZS(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.IsNumber(cp)
}

func charZKoctalZKdigitZS(ch interface{}) interface{} {
	cp := ch.(rune)
	return ((48 <= cp) && (55 <= cp))
}

func charZKpunctuationZS(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.IsPunct(cp)
}

func charZKsymbolicZS(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.IsSymbol(cp)
}

func charZKtitleZKcaseZS(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.IsTitle(cp)
}

func charZKtitlecase(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.ToTitle(cp)
}

func charZKupcase(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.ToUpper(cp)
}

func charZKupperZKcaseZS(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.IsUpper(cp)
}

func charZKwhitespaceZS(ch interface{}) interface{} {
	cp := ch.(rune)
	return unicode.IsSpace(cp)
}

func charZPZQZS(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (rest[0].(rune) <= rest[1].(rune))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(rest[i].(rune) <= rest[(i+1)].(rune)) {
			return false
		}
	}
	return true
}

func charZPZS(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (rest[0].(rune) < rest[1].(rune))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(rest[i].(rune) < rest[(i+1)].(rune)) {
			return false
		}
	}
	return true
}

func charZQZS(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (rest[0].(rune) == rest[1].(rune))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(rest[i].(rune) == rest[(i+1)].(rune)) {
			return false
		}
	}
	return true
}

func charZRZQZS(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (rest[0].(rune) >= rest[1].(rune))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(rest[i].(rune) >= rest[(i+1)].(rune)) {
			return false
		}
	}
	return true
}

func charZRZS(rest ...interface{}) interface{} {
	switch len(rest) {
	case 0:
		panic("no arguments")
	case 1:
		panic("nothing to compare to")
	case 2:
		return (rest[0].(rune) > rest[1].(rune))
	}

	for i := 0; i < (len(rest) - 1); i++ {
		if !(rest[i].(rune) > rest[(i+1)].(rune)) {
			return false
		}
	}
	return true
}

func charZS(a interface{}) interface{} {
	_, ok := a.(rune)
	return ok
}

func digitZKvalue(ch interface{}) interface{} {
	cp := ch.(rune)
	if (48 <= cp) && (cp <= 57) {
		return (int64(cp) - 48)
	}
	return false
}

func hexZKdigitZKvalue(ch interface{}) interface{} {
	cp := ch.(rune)
	if (48 <= cp) && (cp <= 57) {
		return (int64(cp) - 48)
	}
	if (65 <= cp) && (cp <= 70) {
		return (int64(cp) - 55)
	}
	if (97 <= cp) && (cp <= 102) {
		return (int64(cp) - 87)
	}
	return false
}

func integerZKZRchar(cp interface{}) interface{} {
	return rune(cp.(int64))
}
