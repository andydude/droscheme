#!/bin/sh
ECHO="/bin/echo"
NOW="$(date '+%Y%m%dT%H%M%S')"
FILES="$(ls *ss | grep -v '^test')"
#FILES="test-choice-*ss"
#FILES="inexacteq.ss test-choice-negative-zero.ss"
#IMPLS="guile racket kawa sisc mit gauche chibi ds"
#IMPLS="kawa sisc"
IMPLS="ds"

function colorize () {
    sed -e "s/PASS/$(cat test-pass.txt)/g" | sed -e "s/FAIL/$(cat test-fail.txt)/g"
}

function run () {
    $ECHO '----' "$@"
    OUT="$($@ 2>&1 | grep -v '^[^-]')"
    if test $? -eq 0 ; then
        $ECHO "$OUT" | colorize
    else
        $ECHO "$OUT" >> test-output-${NOW}.txt
        $ECHO '-- [FAIL] (see test-output-'${NOW}'.txt for details)' | colorize
    fi
}

for IMPL in $IMPLS ; do
    $ECHO "------ $IMPL ------"
    for FILE in $FILES ; do
        run ./test-$IMPL.sh $FILE
    done
done