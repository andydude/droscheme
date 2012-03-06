GOYACC = goyacc
all: parse.go
parse.go: parse.y
	$(GOYACC) -o $@ $<