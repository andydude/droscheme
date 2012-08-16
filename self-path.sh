#!/bin/bash
#DROSCHEME_PATH="$HOME/.droscheme"
DROSCHEME_PATH="$GOPATH/src/droscheme"
if [ ! -d $DROSCHEME_PATH ]; then
    mkdir -p $DROSCHEME_PATH
fi

export PATH="$DROSCHEME_PATH/bin:$PATH"
export GOPATH="$DROSCHEME_PATH"
cd $DROSCHEME_PATH
