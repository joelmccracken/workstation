# Foundation settings
# This is the kind of thing that sets up the "foundation" for everything else.
# #+name: workstation_foundation

# [[file:../../workstation.org::workstation_foundation][workstation_foundation]]
export WORKSTATION_DIR="$HOME/workstation"
export WORKSTATION_HOST_SETTINGS_SRC_DIR=$WORKSTATION_DIR/hosts/$WORKSTATION_NAME
export WORKSTATION_HOST_CURRENT_SETTINGS_DIR=$WORKSTATION_DIR/hosts/current

if [ -z "${WORKSTATION_NAME+x}" ] ; then
    if [ -f "$WORKSTATION_HOST_CURRENT_SETTINGS_DIR/settings.sh" ]; then
       source "~/$WORKSTATION_HOST_CURRENT_SETTINGS_DIR/settings.sh"
    fi
fi
# workstation_foundation ends here
