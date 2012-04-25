#!/bin/sh
echo 'y' | mit-scheme --batch-mode --load 'test.ss' --load $1 --eval '(exit)' | grep -v Kill
