#!/usr/bin/env bash


# External Script:

# [[file:../../../workstation.org::*Install Nix][Install Nix:2]]
source ~/workstation/lib/shell/setup/workstation_setup_versions.sh
source ~/workstation/lib/shell/funcs.sh

function ensure_nix_installed () {
    if which nix > /dev/null; then
        info "nix exists in path, not installing"
    else
        info "nix not in path, installing"
        sh <(curl -L https://releases.nixos.org/nix/$WORKSTATION_NIX_PM_VERSION/install) --daemon;
    fi
}
ensure_nix_installed
# Install Nix:2 ends here
