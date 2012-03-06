include $(GOROOT)/src/Make.inc

TARG = droscheme
GOFILES = \
	any.go \
	parse.go

include $(GOROOT)/src/Make.pkg
