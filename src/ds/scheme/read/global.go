package ds_scheme_read

var (
	//    (eof-object)
	gEOF = eofZKobject().(rune)

	//    (make-error-object)
	gEOL = makeZKerrorZKobject("end-of-line").(error)
)
