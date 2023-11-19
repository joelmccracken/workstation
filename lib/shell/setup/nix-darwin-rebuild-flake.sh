#!/usr/bin/env bash
# command to have darwin build and switch to next generation

# [[file:../../../workstation.org::*command to have darwin build and switch to next generation][command to have darwin build and switch to next generation:1]]
set -u # error in case WORKSTATION_NAME is not set

nix build --extra-experimental-features "nix-command flakes" \
    ~/workstation\#darwinConfigurations.${WORKSTATION_NAME}.system
./result/sw/bin/darwin-rebuild switch --flake ~/workstation#${WORKSTATION_NAME}

rm -rf ./result
# command to have darwin build and switch to next generation:1 ends here
