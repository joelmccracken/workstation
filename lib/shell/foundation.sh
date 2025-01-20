# Foundation settings
# This is the kind of thing that sets up the "foundation" for everything else.
# #+name: workstation_foundation

# [[file:../../workstation.org::workstation_foundation][workstation_foundation]]
export WORKSTATION_DIR="${WORKSTATION_DIR:-$HOME/workstation}"
export WORKSTATION_EMACS_CONFIG_DIR=$HOME/.config/emacs
export WORKSTATION_GIT_ORIGIN='git@github.com:joelmccracken/workstation.git'
export WORKSTATION_GIT_ORIGIN_PUB='https://github.com/joelmccracken/workstation.git'
export WORKSTATION_HOST_CURRENT_SETTINGS_DIR=$WORKSTATION_DIR/hosts/current

sourceIfExists () {
    if [ -f "$1" ]; then
        source "$1"
    fi
}

if [ -z "${WORKSTATION_NAME+x}" ] ; then
    sourceIfExists "$WORKSTATION_HOST_CURRENT_SETTINGS_DIR/settings.sh"
fi


if [ -z "${WORKSTATION_NAME+x}" ] ; then
    echo WARNING: no environment variable WORKSTATION_NAME provided.
    echo This variable should be exported by a script at:
    echo $WORKSTATION_DIR/hosts/current/settings.sh
    echo see workstation.org for more information
else
    export WORKSTATION_HOST_SETTINGS_SRC_DIR=$WORKSTATION_DIR/hosts/$WORKSTATION_NAME
fi
# workstation_foundation ends here
