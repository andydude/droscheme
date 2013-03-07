#!/bin/bash

function echo_makefile() {
    filename="$1"
    parent="$2"
    cat <<EOF
LIB = ${filename}

FILES = \\
	\$(LIB).gos \\
	\$(LIB).go

include \$(DROSCHEME_PATH)/src.mk
EOF
}

function echo_library() {
    name="$1"
    cat <<EOF
(define-library
 (${name})
 (import)
 (export)
 (begin

  ;; put procedures here

 )
)
EOF
}

function usage() {
    cat <<EOF
Usage:
    cd src
    ../src-skel.sh scheme/library/name
    cd scheme/library/name
    # edit your new library
EOF
}

if [ $# -ne 1 ]; then
    usage
    exit 1
fi

filepath="$1"
cd "${DROSCHEME_PATH}/src"
filename="$(/bin/echo -n ${filepath} | tr '/' '_')"
name="$(/bin/echo -n ${filepath} | ds demangle - | tr '/' ' ')"
filepath_check="$(/bin/echo -n ${name} | ds mangle - | tr ' ' '/' )"

# mangle check
if [ x"${filepath}" != x"${filepath_check}" ]; then
    echo "Error: Expected a mangled name, but got a unmangled name instead."
    echo "Please run this tool again as:"
    echo "    $0 ${filepath_check}"
    exit 1
fi

# make directory
mkdir -p "${filepath}"

# write Makefile
path_makefile="${filepath}/Makefile"
echo makefile '>' "${path_makefile}"
echo_makefile "${filename}" > "${path_makefile}"

# write library
path_library="${filepath}/${filename}.sld"
if [ -e "${path_library}" ]; then
    #echo "Warning: Library definition (${filename}.sld) already exists."
    exit 0
else
    echo library '>>' "${path_library}"
    echo_library "${name}" >> "${path_library}"
fi
