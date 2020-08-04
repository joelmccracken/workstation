#!/usr/bin/env bash
set -euo pipefail

# at this point, emacs is assumed installed
#
INSTALL_DIR=${DOOM_INSTALL_DIR:-~/.emacs.d/doom}
BIN_DIR="${INSTALL_DIR}/bin"

# rm -rf ~/.emacs.d
# mkdir -p ~/.emacs.d
#
ls -lah ~/.emacs.d/

git clone --depth 1 https://github.com/joelmccracken/doom-emacs.git $INSTALL_DIR

env

ls ~/.nix-profile/bin

DOOMDIR=$INSTALL_DIR/doom_d $BIN_DIR/doom -y install
# always fails on package orgit, but just running command again fixes it
DOOMDIR=$INSTALL_DIR/doom_d $BIN_DIR/doom -y install
