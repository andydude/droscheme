#!/bin/bash
cd $(dirname $0)
export DROSCHEME_PATH=$(pwd)
export GOPATH=$DROSCHEME_PATH
cd $DROSCHEME_PATH/src

. $DROSCHEME_PATH/src-pkgs.sh
for PKG in $PKGS ; do
    printf "installing library $PKG\n"
    cd $PKG
    go install
    cd - > /dev/null
done

. $DROSCHEME_PATH/src-cmds.sh
for CMD in $CMDS ; do
    printf "installing command $CMD\n"
    cd $CMD
    go install
    cd - > /dev/null
done
