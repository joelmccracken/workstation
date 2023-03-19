#!/usr/bin/env sh

bw_unlock () {
    # authtenticates bitwarden for this shell session only
    export BW_SESSION=`bw unlock --passwordfile ~/secrets/bw_pass --raw`;
}
