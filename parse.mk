GOYACC = go tool yacc
all: parse.go
parse.go: parse.y
	$(GOYACC) -o $@ $<