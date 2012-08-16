#!/bin/bash
BASE="$1"
BASE="${BASE%.sld}"
BASE="${BASE%.ss}"
BASE="${BASE%.go}"
echo $BASE
./gosc --lib ${BASE}.sld > ${BASE}.go.test
diff -u ${BASE}.go ${BASE}.go.test