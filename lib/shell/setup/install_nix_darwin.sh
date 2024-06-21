#!/usr/bin/env bash
# Install nix-darwin

# [[file:../../../workstation.org::*Install nix-darwin][Install nix-darwin:1]]
source ${WORKSTATION_DIR}/lib/shell/foundation.sh
source ${WORKSTATION_DIR}/lib/shell/setup/workstation_setup_versions.sh

cd $WORKSTATION_DIR
nix-build https://github.com/LnL7/nix-darwin/archive/${WORKSTATION_NIX_DARWIN_VERSION}.tar.gz -A installer
./result/bin/darwin-installer

source ${WORKSTATION_DIR}/lib/shell/setup/nix-darwin-rebuild-flake.sh
# Install nix-darwin:1 ends here
