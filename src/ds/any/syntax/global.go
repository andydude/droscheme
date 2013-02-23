package ds_any_syntax

var (
	//     _eofZKobjectZS
	gEOF = _eofZKobject()
	gEOL = _errorZKobject("end-of-line").(error)
)