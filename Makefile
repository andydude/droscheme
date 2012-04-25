all:
#	make -f parse.mk
#	go build
	go install
	cd cmd/ds ; go build ; cd ../..
