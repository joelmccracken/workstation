#!/usr/bin/env bash
# library of shell functions

# [[file:../../workstation.org::*library of shell functions][library of shell functions:1]]
function is_mac() {
    [[ "$(uname)" == 'Darwin' ]]
}

function is_linux() {
    [[ "$(uname)" == 'Linux' ]]
}

function info() {
    echo "INFO ========= $(date) $@"
}

# unlocks bitwarden, so that the `bw` program can access the bitwarden database.
bw_unlock () {
    # authtenticates bitwarden for this shell session only
    export BW_SESSION=`bw unlock --passwordfile ~/secrets/bw_pass --raw`;
}
# library of shell functions:1 ends here
