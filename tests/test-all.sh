#!/bin/sh
ECHO="/bin/echo"
NOW="$(date '+%Y%m%dT%H%M%S')"
#FILES="$(ls *ss | grep -v '^test')"
FILES="test-choice-*ss"
#FILES="inexacteq.ss test-choice-negative-zero.ss"
IMPLS="guile mit racket ds"
#IMPLS="ds"

function run () {
    $ECHO '----' "$@"
    OUT="$($@ 2>&1 | grep -v '^[^-]')"
    if test $? -eq 0 ; then
        if ($ECHO "$OUT" | grep FAIL >> /dev/null) ; then
            $ECHO "[31m$OUT[0m"
        else
            $ECHO "[32m$OUT[0m"
        fi
    else
        $ECHO "$OUT" >> test-output-${NOW}.txt
        $ECHO '[31m-- [FAIL] (see test-output-'${NOW}'.txt for details)[0m'
    fi
}

for IMPL in $IMPLS ; do
    $ECHO "------ $IMPL ------"
    for FILE in $FILES ; do
        run ./test-$IMPL.sh $FILE
    done
done