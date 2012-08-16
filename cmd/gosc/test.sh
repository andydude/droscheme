#!/bin/bash
DIR="$(dirname $0)"
for X in "$@" ; do
	case "$X" in
        (*.sld) $DIR/t-lib.sh "$X" ;;
        (*.ss) $DIR/t-main.sh "$X" ;;
    esac
done
