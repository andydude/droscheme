#!/bin/bash
ECHO=/bin/echo

if which go 2> /dev/null; then
    GO=go
else
    $ECHO "ERROR: you must have Go version 1 or higher installed!"
    exit 1
fi

if which guile 2> /dev/null; then
    GUILE=guile
else
    $ECHO "ERROR: you must have Guile version 2 or higher installed!"
    exit 1
fi

if which wget 2> /dev/null; then
    WEBGET="wget --no-check-certificate -O -"
else
    if which curl 2> /dev/null; then
        WEBGET="curl -o -"
    else
        $ECHO "ERROR: you must have either wget or curl installed!"
        exit 1
    fi
fi

if which tar 2> /dev/null; then
    TAR=tar
    if which gzip 2> /dev/null; then
        GZIP=gzip
    else
        $ECHO "ERROR: you must have both tar and gzip installed!"
        exit 1
    fi
else
    $ECHO "ERROR: you must have both tar and gzip installed!"
    exit 1
fi

if which git 2> /dev/null; then
    GIT=git
else
    $ECHO "ERROR: you must have git installed!"
    exit 1
fi

# OLD STUFF
# variables
#TARFILE="https://github.com/andydude/droscheme/tarball/master"
#ZIPFILE="https://github.com/andydude/droscheme/zipball/master"
# download
#TEMP=".temp"
#mkdir -p $TEMP
#$WEBGET $TARFILE | $TAR -xzf - -C $TEMP
#TEMPDIR=$(ls $TEMP)
#mv $TEMP/$TEMPDIR $DROSCHEME_PATH
#rmdir $TEMP
#cd $DROSCHEME_PATH
# END OLD STUFF
#cd $DROSCHEME_PATH

GITREPO="git://github.com/andydude/droscheme.git"
export DROSCHEME_PATH=$HOME/.droscheme
echo mkdir -p $DROSCHEME_PATH
mkdir -p $DROSCHEME_PATH
if test ! -d $DROSCHEME_PATH; then
    echo git clone $GITREPO $DROSCHEME_PATH
    git clone $GITREPO $DROSCHEME_PATH
fi

# install
echo bash $DROSCHEME_PATH/src-install.sh
bash $DROSCHEME_PATH/src-install.sh

# message
$ECHO "---"
$ECHO "Install Complete."
$ECHO "Please add" 'export PATH="$PATH:'$DROSCHEME_PATH'/bin"' "to your bash profile."
$ECHO "After doing this, you can reach the Droscheme prompt by typing: 'dsi'."
$ECHO "---"
