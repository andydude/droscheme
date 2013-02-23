FILES = \
	scheme_processZKcontext.go

all: $(FILES)

%.sld: %.ild
	gosc --sugar $< > $@

%.go: %.sld
	gosc --lib $< > $@
