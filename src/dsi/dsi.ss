#!/usr/bin/env ds -f
(define g-args (command-line))
(define g-env (current-environment))

; prevent parsing command line
(exit)