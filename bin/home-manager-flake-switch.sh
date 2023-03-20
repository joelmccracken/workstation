#!/usr/bin/env bash
# flake-world equivalent to home-manager 'switch'
# The pre-flake way of using home manager had a ~home-manager switch~ command
# which would build and then activate the next home manager generation. This is
# the flake "equivalent". Having it as a shell command makes it easier to run.

# Also, this script obviously requires the ~WORKSTATION_NAME~ environment variable
# to be set, which provides the 'identity' of the current machine -- not all
# machines have the same home manager configurations.

# [[file:../workstation.org::*flake-world equivalent to home-manager 'switch'][flake-world equivalent to home-manager 'switch':1]]
set -u # error in case WORKSTATION_NAME is not set
function home_manager_flake_switch() {
    nix build --no-link ~/workstation/#homeConfigurations.${WORKSTATION_NAME}.$(whoami).activationPackage
    "$(nix path-info ~/workstation/#homeConfigurations.${WORKSTATION_NAME}.$(whoami).activationPackage)"/activate
}
home_manager_flake_switch
# flake-world equivalent to home-manager 'switch':1 ends here
