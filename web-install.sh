#!/bin/bash
ECHO=$(which echo)

function SAY () {
	$ECHO "$@"
	"$@"
}

function CHECK_PROG () {
	if which $1 2> /dev/null; then
		:
	else
	    $ECHO "ERROR: you must have $2 installed!"
	    exit 1
	fi
}

CHECK_PROG bash "Bash"
CHECK_PROG git "Git"
CHECK_PROG go "Go (version >= 1)"
CHECK_PROG guile "Guile (version >= 2)"

# variables
GITARGS="--branch andrew-working"
GITREPO="git://github.com/andydude/droscheme.git"
SAY export DROSCHEME_PATH=$HOME/.droscheme
SAY mkdir -p $DROSCHEME_PATH

# download
if test ! -e $DROSCHEME_PATH/src-install.sh; then
    SAY git clone $GITARGS $GITREPO $DROSCHEME_PATH
fi

# install
SAY bash $DROSCHEME_PATH/src-install.sh

# message
$ECHO "---"
$ECHO "Install Complete."
$ECHO "Please add" 'export PATH="$PATH:'"$DROSCHEME_PATH"'/bin"' "to your bash profile."
$ECHO "After doing this, you can reach the Droscheme prompt by typing: 'dsi'."
$ECHO "---"
