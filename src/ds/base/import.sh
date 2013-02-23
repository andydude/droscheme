#!/bin/bash
cat import.go | grep -o '"[^"]*"' | cut -d'"' -f2 | grep -v '/' | sort -u
