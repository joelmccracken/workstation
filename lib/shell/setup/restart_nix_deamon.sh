#!/usr/bin/env bash


# External Script:

# [[file:../../../workstation.org::*restart nix daemons][restart nix daemons:2]]
source ~/workstation/lib/shell/funcs.sh
function restart_nix_daemon_linux() {
    sudo systemctl restart nix-daemon.service;
}

function restart_nix_daemon_mac() {
    set +e
    sudo launchctl unload /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    set -e
}

function restart_nix_daemon () {
    is_mac && restart_nix_daemon_mac
    is_linux && restart_nix_daemon_linux
}
restart_nix_daemon
# restart nix daemons:2 ends here
