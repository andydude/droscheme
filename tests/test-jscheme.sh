#!/bin/sh
java -cp '/opt/schemes/jscheme/lib/*' jscheme.REPL '(load "test.ss")' $1
