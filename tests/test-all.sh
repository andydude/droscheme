#!/bin/sh
ECHO="/bin/echo"
NOW="$(date '+%Y%m%dT%H%M%S')"
FILES="$(ls *ss | grep -v '^test')"
#IMPLS="chibi gauche guile mit racket ds"
IMPLS="ds"

function run () {
    $ECHO '----' "$@"
    OUT="$($@ 2>&1)"
    if ($ECHO "$OUT" | grep '^--') ; then
        #$ECHO "$OUT"
        :
    else
        $ECHO "$OUT" >> test-output-${NOW}.txt
        $ECHO '-- [FAIL] (see test-output-'${NOW}'.txt for details)'
    fi
}

for IMPL in $IMPLS ; do
    $ECHO "------ $IMPL ------"
    for FILE in $FILES ; do
        run ./test-$IMPL.sh $FILE
    done
done