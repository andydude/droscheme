FILES = \
	ds_any_env_null.sld \
	ds_any_env_null.go

all: $(FILES)

%.sld: %.ild
	gosc --sugar $< > $@

%.go: %.sld
	gosc --lib $< > $@
