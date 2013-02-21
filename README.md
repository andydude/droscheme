droscheme
=========

Droscheme - a Scheme implementation.

The goals of Droscheme are to provide a complete numerical tower,
a complete I/O port system including binary and textual ports,
and to experiment with nonstandard numbers and ports.

Dependancies
============

Droscheme requires Go. For an easy install process, Git is recommended.
For bootstrapping from sources, either Guile or Droscheme is required.

Easy Install
============

To install on Linux, run the command:

 wget https://raw.github.com/andydude/droscheme/master/web-install.sh \
      --no-check-certificate -O - | bash -

To install on Mac OS X, run the command:

 curl https://raw.github.com/andydude/droscheme/master/web-install.sh \
      -o - | bash -

Windows is not currently supported.

Manual Install
==============

You may install manually, but the process is more involved.
You have to download the sources, with git, .tar.gz, or zip,
and you can expand the directory whereever you want.

As long as you have $DROSCHEME_PATH set, it should work.
After the directory is expanded, make sure Go is installed,
and run $DROSCHEME_PATH/src-install.sh, which simply runs

 go install

in every directory.
