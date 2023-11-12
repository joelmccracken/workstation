#!/usr/bin/env bash
# [[file:../../../workstation.org::*Install home manager][Install home manager:2]]
function install_home_manager() {
    export HOME_MANAGER_BACKUP_EXT=old

    nix run home-manager/$WORKSTATION_HOME_MANAGER_VERSION -- init ~/workstation
}

install_home_manager
# Install home manager:2 ends here
