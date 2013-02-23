FILES = \
	ds_any_runtime.sld \
	ds_any_runtime.go

all: $(FILES)

#%.sld: %.ild
#	gosc --sugar $< > $@

%.go: %.sld
	gosc --lib $< > $@

%.go: %.sld
	gosc --lib $< > $@
