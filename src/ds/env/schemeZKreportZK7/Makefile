FILES = \
	ds_any_env_schemeZKreportZK7.sld \
	ds_any_env_schemeZKreportZK7.go

all: $(FILES)

%.sld: %.ild
	gosc --sugar $< > $@

%.go: %.sld
	gosc --lib $< > $@
