CAT = cat
TAC = gtac

all: gos.html

%.html: %.scrib gos-style.css template-begin.html template-end.html
	scribble --html $<
	@mv $@ $@.1.temp
#	@echo '@import url("gos-style.css");' > scribble-style.css
	xmllint --html --xmlout $@.1.temp > $@.2.temp 2> /dev/null
	xmlindent $@.2.temp > $@.3.temp
	tail -n +10 $@.3.temp | $(TAC) > $@.4.temp
	tail -n +4 $@.4.temp | $(TAC) > $@.5.temp
	$(CAT) template-begin.html $@.5.temp template-end.html > $@
