#!/usr/bin/env bash
# setting directories for current host
# Each workstaion host I use has different settings needs. My remote cloud hosted
# server has a different setup than my mac laptop, which has a different set up
# from my work computer. The way I have these settings specified is by having a
# directory in my home directory which has all of the needed files to
# specify such differences. There are different directories for each host I
# maintain, but on a specific, individual host, one of those directories are symlinked
# into the 'current' (src_text{~/workstation/hosts/current/})
# directory, and various workstation components are programmed to look
# in this location for their code/settings as appropriate.


# [[file:../../../workstation.org::*setting directories for current host][setting directories for current host:1]]
set -e

usage() {
    cat <<-EOF
usage: $0 <WORKSTATION_NAME>

Sets up expected link in a 'current' directory, based upon the WORKSTATION_NAME
provided as argument. Environment variable WORKSTATION_DIR is must also be set so
script can put link within.
EOF
}

if [ "$#" -ne 1 ]; then
    echo "Error: Wrong number of arguments"
    usage
    exit 55
fi

if [ "${WORKSTATION_DIR:-}" = "" ]; then
    echo "Error: environment variable WORKSTATION_DIR unset"
    usage
    exit 56
fi

source ${WORKSTATION_DIR}/lib/shell/foundation.sh
source ${WORKSTATION_DIR}/lib/shell/funcs.sh

WORKSTATION_NAME="$1"

export WORKSTATION_HOST_CURRENT_SETTINGS_DIR=$WORKSTATION_DIR/hosts/current
export WORKSTATION_HOST_SETTINGS_SRC_DIR=$WORKSTATION_DIR/hosts/$WORKSTATION_NAME

info setting current host settings directory
info workstation host settings directory: $WORKSTATION_HOST_SETTINGS_SRC_DIR

if [ -d "$WORKSTATION_HOST_CURRENT_SETTINGS_DIR" ]; then
    info "$WORKSTATION_HOST_CURRENT_SETTINGS_DIR" already exists, not changing. \
         If this is incorrect, run rm "$WORKSTATION_HOST_CURRENT_SETTINGS_DIR" \
         and then rerun this script
    exit 120
fi

if [ -d $WORKSTATION_HOST_SETTINGS_SRC_DIR ]; then
    info setting current host directory to $WORKSTATION_HOST_SETTINGS_SRC_DIR;
    ln -s $WORKSTATION_HOST_SETTINGS_SRC_DIR $WORKSTATION_HOST_CURRENT_SETTINGS_DIR;
else
    echo ERROR $WORKSTATION_HOST_SETTINGS_SRC_DIR does not exist, cannot link
    exit 5
fi
# setting directories for current host:1 ends here
