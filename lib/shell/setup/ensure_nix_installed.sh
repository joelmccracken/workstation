#!/usr/bin/env bash
# Install Nix

# [[file:../../../workstation.org::*Install Nix][Install Nix:1]]
source ~/workstation2/lib/shell/setup/workstation_setup_versions.sh
source ~/workstation2/lib/shell/funcs.sh

if which nix > /dev/null; then
    info "nix exists in path, not installing"
else
    info "nix not in path, installing"
    sh <(curl -L https://releases.nixos.org/nix/$WORKSTATION_NIX_PM_VERSION/install) --daemon;
fi
# Install Nix:1 ends here
