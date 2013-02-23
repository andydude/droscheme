FILES = \
	ds_num_runtime.sld \
	ds_num_runtime.go

all: $(FILES)

%.sld: %.ild
	gosc --sugar $< > $@

%.go: %.sld
	gosc --lib $< > $@
