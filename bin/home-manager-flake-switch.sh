#!/usr/bin/env bash
# flake-world equivalent to home-manager 'switch'
# The pre-flake way of using home manager had a ~home-manager switch~ command
# which would build and then activate the next home manager generation. This is
# the flake "equivalent". Having it as a shell command makes it easier to run.

# [[file:../workstation.org::*flake-world equivalent to home-manager 'switch'][flake-world equivalent to home-manager 'switch':1]]
function home_manager_flake_switch() {
    nix build --no-link ~/workstation/#homeConfigurations.${WORKSTATION_NAME}.$(whoami).activationPackage
    "$(nix path-info ~/workstation/#homeConfigurations.${WORKSTATION_NAME}.$(whoami).activationPackage)"/activate
}
home_manager_flake_switch
# flake-world equivalent to home-manager 'switch':1 ends here
