all: $(FILES)
	go build

clean:
	rm -f $(FILES)

install: all
	go install

import.gos: $(LIB).sld
	ds scheme2gos import $(LIB).sld > import.gos

export.gos: $(LIB).sld
	ds scheme2gos export $(LIB).sld > export.gos

main.scm: main.icm
	ds i2s $< $@

main.gos: main.scm
	ds scheme2gos program $< $@

%.sld: %.ild
	ds i2s $< $@

%.gos: %.sld
	ds scheme2gos library $< $@

%.go: %.gos
	ds gos2go $< $@
	go fmt
