all:
	make -f parse.mk
	make -f ds.mk
#	go build
	go install
	cd cmd/ds ; go build ; cd ../..
