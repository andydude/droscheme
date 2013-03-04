#!/bin/sh
echo '(exit #t)' | /opt/local/bin/sisc "test.ss" "$1" | grep '\--'


