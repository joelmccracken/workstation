#!/usr/bin/env bash


# Which requires a shell script:


# [[file:../workstation.org::*Makefile][Makefile:2]]
EMACS_CONFIG_DIR=~/.config/emacs;
$EMACS_CONFIG_DIR/bin/doomscript lib/emacs/tangle-file.el
# Makefile:2 ends here
