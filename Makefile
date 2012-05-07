all:
#	make -f parse.mk
#	go build
	go install
#	cd dsbufio ; go install ; cd ..
	cd cmd/ds ; go build ; cd ../..
