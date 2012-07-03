all: gos.html

%.html: %.scrib gos-style.css
	scribble --html $<
	@echo '@import url("gos-style.css");' > scribble-style.css
	@mv $@ $@.1.temp
	xmllint --html --xmlout $@.1.temp | grep -v DOCTYPE | grep -v '<?xml' > $@.2.temp
	xmlindent $@.2.temp > $@.3.temp
	cat template-begin.html $@.3.temp template-end.html > $@
#	xsh -l $@.2.temp $@.3.temp
#	xmllint --html --htmlout $@.3.temp > $@