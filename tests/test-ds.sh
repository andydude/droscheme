#!/bin/sh
ds -e '(begin (load "test.ss" (current-environment)) (load "'$1'" (current-environment)))'