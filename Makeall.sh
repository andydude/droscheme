#!/bin/sh

make -f parse.mk

# build library
make

# build tools
cd tools
make
cd ..
