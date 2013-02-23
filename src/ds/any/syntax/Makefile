FILES = \
	ds_any_syntax.sld \
	ds_any_syntax.go

all: $(FILES)

%.sld: %.ild
	gosc --sugar $< > $@

%.go: %.sld
	gosc --lib $< > $@
