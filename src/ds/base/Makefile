LIB = ds_base

FILES = \
	$(LIB).sld \
	$(LIB).go \
	export.go \
	import.go

all: $(FILES)
	go build

clean:
	rm $(FILES)

install: all
	go install

export.go: $(LIB).sld
	gosc --export $(LIB).sld > export.go

import.go: $(LIB).sld
	gosc --import $(LIB).sld > import.go

%.sld: %.ild
	gosc --sugar $< > $@

%.go: %.sld
	gosc --lib $< > $@

