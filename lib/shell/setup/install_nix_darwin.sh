#!/usr/bin/env bash
# [[file:../../../workstation.org::*Install nix-darwin][Install nix-darwin:2]]
source ~/workstation/lib/shell/foundation.sh
source ~/workstation/lib/shell/setup/workstation_setup_verions.sh

function nix_darwin_rebuild_flake() {
    nix build --extra-experimental-features "nix-command flakes" \
        ~/workstation\#darwinConfigurations.${WORKSTATION_NAME}.system
    ./result/sw/bin/darwin-rebuild switch --flake ~/workstation#${WORKSTATION_NAME}

    rm -rf ./result
}

function install_nix_darwin() {
    cd $WORKSTATION_DIR
    nix-build https://github.com/LnL7/nix-darwin/archive/${WORKSTATION_NIX_DARWIN_VERSION}.tar.gz -A installer
    ./result/bin/darwin-installer

    nix_darwin_rebuild_flake
}

install_nix_darwin
# Install nix-darwin:2 ends here
