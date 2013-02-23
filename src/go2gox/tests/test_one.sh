#!/bin/bash
NAME=$1
ECHO=/bin/echo
$ECHO "go2gos"
go2gos ${NAME}.go > ${NAME}.gos.4.temp
$ECHO "gos2go"
gos2go ${NAME}.gos.4.temp > ${NAME}.go.3.temp
$ECHO "gosemi"
gosemi ${NAME}.go.3.temp | gofmt > ${NAME}.go.2.temp
$ECHO "gosemi"
gosemi ${NAME}.go | gofmt > ${NAME}.go.1.temp
$ECHO "diff"
diff -u ${NAME}.go.1.temp ${NAME}.go.2.temp
rm -f *.temp