# Foundation settings
# This is the kind of thing that sets up the "foundation" for everything else.
# #+name: workstation_foundation

# [[file:../../workstation.org::workstation_foundation][workstation_foundation]]
export WORKSTATION_DIR="$HOME/workstation"
export WORKSTATION_EMACS_CONFIG_DIR=~/.config/emacs
export WORKSTATION_GIT_ORIGIN='git@github.com:joelmccracken/workstation.git'
export WORKSTATION_GIT_ORIGIN_PUB='https://github.com/joelmccracken/workstation.git'

sourceIfExists () {
    if [ -f "$1" ]; then
        source "$1"
    fi
}

if [ -z "${WORKSTATION_NAME+x}" ] ; then
    if [ -f "$WORKSTATION_HOST_CURRENT_SETTINGS_DIR/settings.sh" ]; then
       source "~/$WORKSTATION_HOST_CURRENT_SETTINGS_DIR/settings.sh"
    fi
fi

if [ -z "${WORKSTATION_NAME+x}" ] ; then
    echo WARNING: no environment variable WORKSTATION_NAME provided.
    echo This variable should be exported by a script at:
    echo ~/$WORKSTATION_DIR/hosts/current/settings.sh
    echo see workstation.org for more information
    echo TODO provide reference to exact location
else
    # only set these if we have a WORKSTATION_NAME. This may be a bad call, perhaps
    export WORKSTATION_HOST_SETTINGS_SRC_DIR=$WORKSTATION_DIR/hosts/$WORKSTATION_NAME
    export WORKSTATION_HOST_CURRENT_SETTINGS_DIR=$WORKSTATION_DIR/hosts/current
fi
# workstation_foundation ends here
