#!/usr/bin/env bash
# library of shell functions
# This single file contains many of the general-purpose functions that I use in
# numerous scripts etc.

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

function polite-git-checkout () {
    DIR=$1
    REPO=$2

    cd $DIR
    git init
    git remote add origin $REPO
    git fetch

    # wont work (it will have already been deleted from the index)
    git reset --mixed origin/master
    # This formulation of the checkout command seems to work most reliably
    git status -s | grep -E '^ D' | sed -E 's/^ D //' | xargs -n 1 -- git checkout
}

function mv_dated_backup() {
    local THEDIR="$1"
    if test -e "$THEDIR"; then
        mv "$THEDIR" "${THEDIR}-$(date +"%s")"
    fi
}

function is_git_repo_cloned_at(){
    cd $1 && [[ "$(git remote get-url origin)" == "$2" ]]
}

function clone_repo_and_checkout_at() {
    mv_dated_backup $1
    info cloning repo into $1
    git clone $2 $1
    cd $1
    info checking out commit $3
    git checkout $3
}
# library of shell functions:1 ends here
