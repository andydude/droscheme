#!/bin/sh
chibi-scheme -e '(begin (load "test.ss") (load "'$1'"))'