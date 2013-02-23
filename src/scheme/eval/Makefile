FILES = \
	scheme_eval.go

all: $(FILES)

%.sld: %.ild
	gosc --sugar $< > $@

%.go: %.sld
	gosc --lib $< > $@
