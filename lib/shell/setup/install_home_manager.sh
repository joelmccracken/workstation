#!/usr/bin/env bash
# Install home manager

# [[file:../../../workstation.org::*Install home manager][Install home manager:1]]
export HOME_MANAGER_BACKUP_EXT=old

nix run home-manager/$WORKSTATION_HOME_MANAGER_VERSION -- init $HOME/workstation
# Install home manager:1 ends here
