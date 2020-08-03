#!/usr/bin/env bash
set -euo pipefail

# handy little script that will let me run a different doom directory and not mess
# up my active doom config.

INSTALL_DIR=${DOOM_INSTALL_DIR:-~/.emacs.d/doom}
DOOMDIR=$INSTALL_DIR/doom_d emacs -l $INSTALL_DIR/init.el --batch --eval "$@"

