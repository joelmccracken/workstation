#!/usr/bin/env bash
# [[file:../../../workstation.org::*Initial Bitwarden Sync][Initial Bitwarden Sync:2]]
source ~/workstation/lib/shell/funcs.sh


# The initial BitWarden Sync process. Requires wshs/bww executable to
# be built and available. This could all be more robust
# extracting it is theoretically useful as it provides a mechanism for
# resetting the secrets.
# Likely this should be broken down into separate functions that can be reused.
function initial_bitwarden_sync() {
    # why is bash so cryptic
    if [ ! -z "${BW_CLIENTID+x}" ] && \
       [ ! -z "${BW_CLIENTSECRET+x}" ] && \
       [ ! -z "${WS_BW_MASTER_PASS+x}" ]; then
        info variables requried to run bww force-sync are set, running
        if [ ! -d ~/secrets ]; then
            mkdir ~/secrets;
        fi
        cd  ~/workstation/wshs
        # overwriting anything that was previously in the file
        echo "${WS_BW_MASTER_PASS}" > ~/secrets/bw_pass
        bw login --apikey
        bw_unlock
        bw sync
        $(nix path-info .#"wshs:exe:bww")/bin/bww force-sync
    else
        info variables required to run bww force sync are MISSING, skipping
    fi
}

initial_bitwarden_sync
# Initial Bitwarden Sync:2 ends here
