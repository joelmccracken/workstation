#!/usr/bin/env bash
# Bootstraping Script
# This script is intended to be entrypoint to this project. It can be curled to a
# new machine and then run, and will set things up on that machine as necessary.

# The steps to the setup are given more details in
# [[*Bootstrap Script Execution Process][Bootstrap Script Execution Process]].

# [[file:workstation.org::*Bootstraping Script][Bootstraping Script:1]]
set -xeuo pipefail

# These are the various versions of things that should be installed. Keeping them
# in one place like this make them easier to keep track of.

export NIX_PM_VERSION=nix-2.11.1
export NIX_DARWIN_VERSION=f6648ca0698d1611d7eadfa72b122252b833f86c
export HOME_MANAGER_VERSION=0f4e5b4999fd6a42ece5da8a3a2439a50e48e486
# Script should be passed a single argument, which is name of this workstation.

# When using script to set up a workstation, the "name" of the workstation should
# be provided as the first argument. This is used to pick which settings should be
# applied to this machine.
if [ -z "${1+x}" ]; then
    echo WORKSTATION_NAME must be provided as first argument
    exit 2
else
    export WORKSTATION_NAME="$1"
fi

# This argument generally should not be used by the user, but it is needed for
# the CI process.
# When the CI process starts, we start out with a check out of the code for this
# commit in a directory on the CI machine. However, this is not how workstation runs:
# - part of the job of workstation is getting its own code from the server
# - workstation expects the code to be in a specific directory, that is, ~/workstation
# Because of this (and possibly other reasons that escape me now), even though the
# source code of the current commit is checked out on the CI machine already,
# the CI process re-downloads the code (via this script). The specific SHA to get
# is passed via the argument below. However, if actually being used by a user,
# generally user will always want to use the most up to date content of the master
# branch, so this can be ignored.
# I think probably this sha should just be passed in as an environment variable
# instead of a CLI argument, as that seems a bit less confusing to me.
if [ -z "${2+x}" ]; then
    export WORKSTATION_BOOTSTRAP_COMMIT=origin/master
else
    export WORKSTATION_BOOTSTRAP_COMMIT="$2"
fi
# having these variables here really just makes the code a bit more DRY
WS_DIR="$HOME/workstation"
export WORKSTATION_HOST_SETTINGS_SRC_DIR=$WS_DIR/hosts/$WORKSTATION_NAME
export WORKSTATION_HOST_CURRENT_SETTINGS_DIR=$WS_DIR/hosts/current
WS_ORIGIN='git@github.com:joelmccracken/workstation.git'
WS_ORIGIN_PUB='https://github.com/joelmccracken/workstation.git'
EMACS_CONFIG_DIR=~/.config/emacs
# hereafter, we use many helper functions. Here they are defined up front,
# as some of them are used throughout the other code.

# [[file:workstation.org::is_mac_function][is_mac_function]]
function is_mac() {
    [[ "$(uname)" == 'Darwin' ]]
}
# is_mac_function ends here

# [[file:workstation.org::is_linux_function][is_linux_function]]
function is_linux() {
    [[ "$(uname)" == 'Linux' ]]
}
# is_linux_function ends here

# [[file:workstation.org::info_function][info_function]]
function info() {
    echo "INFO ========= $(date) $@"
}
# info_function ends here

# [[file:workstation.org::polite_git_checkout_function][polite_git_checkout_function]]
function polite-git-checkout () {
    DIR=$1
    REPO=$2
    ORIGIN=$3

    cd $DIR
    git init
    git remote add origin $REPO
    git fetch

    # wont work (it will have already been deleted from the index)
    git reset --mixed origin/master
    # This formulation of the checkout command seems to work most reliably
    git status -s | grep -E '^ D' | sed -E 's/^ D //' | xargs -n 1 -- git checkout
    # fixing; used public to start, but want to be able to push
    git remote set-url origin $ORIGIN
}
# polite_git_checkout_function ends here

# [[file:workstation.org::mv_dated_backup_function][mv_dated_backup_function]]
function mv_dated_backup() {
    local THEDIR="$1"
    if test -e "$THEDIR"; then
        mv "$THEDIR" "${THEDIR}-$(date +"%s")"
    fi
}
# mv_dated_backup_function ends here

# [[file:workstation.org::is_git_repo_cloned_at_function][is_git_repo_cloned_at_function]]
function is_git_repo_cloned_at(){
    cd $1 && [[ "$(git remote get-url origin)" == "$2" ]]
}
# is_git_repo_cloned_at_function ends here

# [[file:workstation.org::clone_repo_and_checkout_at_function][clone_repo_and_checkout_at_function]]
function clone_repo_and_checkout_at() {
    mv_dated_backup $1
    info cloning repo into $1
    git clone $2 $1
    cd $1
    info checking out commit $3
    git checkout $3
    info setting origin
    git remote set-url origin $4
}
# clone_repo_and_checkout_at_function ends here

# [[file:workstation.org::xcode_setup_function][xcode_setup_function]]
function xcode_setup() {
    # this will accept the license that xcode requires from the command line
    # and also install xcode if required.
    sudo bash -c '(xcodebuild -license accept; xcode-select --install) || exit 0'
}
# xcode_setup_function ends here

# [[file:workstation.org::is_brew_installed_function][is_brew_installed_function]]
function is_brew_installed() {
    which brew > /dev/null
}
# is_brew_installed_function ends here

# [[file:workstation.org::homebrew_setup_function][homebrew_setup_function]]
function homebrew_setup() {
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
}
# homebrew_setup_function ends here

# [[file:workstation.org::update_apt_install_git_function][update_apt_install_git_function]]
function update_apt_install_git() {
    sudo bash -c 'apt-get update && apt-get install git'
}
# update_apt_install_git_function ends here

# [[file:workstation.org::*Bootstraping Script][is_git_repo_cloned_at_function]]
function is_git_repo_cloned_at(){
    cd $1 && [[ "$(git remote get-url origin)" == "$2" ]]
}
# is_git_repo_cloned_at_function ends here

# [[file:workstation.org::*Bootstraping Script][clone_repo_and_checkout_at_function]]
function clone_repo_and_checkout_at() {
    mv_dated_backup $1
    info cloning repo into $1
    git clone $2 $1
    cd $1
    info checking out commit $3
    git checkout $3
    info setting origin
    git remote set-url origin $4
}
# clone_repo_and_checkout_at_function ends here

# [[file:workstation.org::install_doom_emacs_no_nix_function][install_doom_emacs_no_nix_function]]
function install_doom_emacs_no_nix() {
    {
        cd $EMACS_CONFIG_DIR
        [[ "$(git remote get-url origin)" == 'https://github.com/hlissner/doom-emacs' ]]
    } || {
        mv_dated_backup $EMACS_CONFIG_DIR
        time git clone --depth 1 https://github.com/doomemacs/doomemacs $EMACS_CONFIG_DIR/
        # alternative: use this if encounter problems
        # ~/.emacs.d/bin/doom -y install;
        # time timeout 45m bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0
        # time bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0
        time timeout 60m bash -c "yes | $EMACS_CONFIG_DIR/bin/doom install" || exit 0
        $EMACS_CONFIG_DIR/bin/doom sync
        echo FINISHED INSTALLING DOOM;
    }
}
# install_doom_emacs_no_nix_function ends here
info starting workstation bootstrap
is_mac && {
    info ensuring xcode is installed
    xcode_setup
    info finished ensuring xcode is installed

    info ensuring brew is installed
    if ! is_brew_installed; then
        homebrew_setup
    fi
    info finished ensuring brew is installed

    info installing git
    brew install git
    info finished installing git

}
is_linux && {
    info updating apt, installing git
    update_apt_install_git
    info finished updating apt, installing git
}
is_git_repo_cloned_at $WS_DIR $WS_ORIGIN || {
    clone_repo_and_checkout_at $WS_DIR $WS_ORIGIN_PUB \
        $WORKSTATION_BOOTSTRAP_COMMIT $WS_ORIGIN
}
# at this point, this is hardly necessary; however, the gitignore file is handy
# i may explore getting rid of this repo entirely and just having a fresh
# repo without any origin in ~
info ensuring dotfiles repo is checked out

DOTFILES_ORIGIN='git@github.com:joelmccracken/dotfiles.git'

is_git_repo_cloned_at ~ "$DOTFILES_ORIGIN" ||
    polite-git-checkout ~ 'https://github.com/joelmccracken/dotfiles.git' \
        "$DOTFILES_ORIGIN"

info finished ensuring dotfiles repo is checked out
# each workstaion host I use has different settings needs.
# For example, my remote cloud hosted server has a different setup than
# my mac laptop, which has a different set up from my work computer.
# the way I have these settings specified is by having a directory in my home
# directory which has all of the needed files I would need for such differences.
# there are different directories for each host I maintain, but on a given host,
# one of those directories are symlinked into 'current' host, which other things
# can then refer to

info setting current host settings directory...
info workstation host settings directory: $WORKSTATION_HOST_SETTINGS_SRC_DIR

if [ -d $WORKSTATION_HOST_SETTINGS_SRC_DIR ]; then
    info setting current host directory to $WORKSTATION_HOST_SETTINGS_SRC_DIR;
    ln -s $WORKSTATION_HOST_SETTINGS_SRC_DIR $WORKSTATION_HOST_CURRENT_SETTINGS_DIR;
else
    echo ERROR $WORKSTATION_HOST_SETTINGS_SRC_DIR does not exist, must exit
    exit 5
fi
info ensuring nix is installed
{ which nix > /dev/null; } || {
    info installing nix
    sh <(curl -L https://releases.nixos.org/nix/$NIX_PM_VERSION/install) --daemon;
}
info finished ensuring nix is installed

export NIX_REMOTE=daemon

info setting up nix.conf
(sudo bash -c 'mkdir -p /etc/nix; cat > /etc/nix/nix.conf') <<-EOF
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
substituters = https://cache.nixos.org https://cache.iog.io
experimental-features = nix-command flakes
trusted-users = root $(whoami) runner
build-users-group = nixbld
# END OF /etc/nix/nix.conf
EOF


function restart_linux_daemon() {
    sudo systemctl restart nix-daemon.service;
}

function restart_mac_daemon() {
    set +e
    sudo launchctl unload /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    set -e
}

is_mac && {
    info macos detected, restarting nix-daemon
    restart_mac_daemon
    info finished restarting nix-daemon
}

is_linux && {
    info restarting nix-daemon via systemctl
    restart_linux_daemon
    info finished restarting nix-daemon via systemctl
}

NIX_DAEMON_PATH='/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
cat $NIX_DAEMON_PATH
if [[ -e "$NIX_DAEMON_PATH" ]]; then
    set +u
    source "$NIX_DAEMON_PATH";
    set -u
fi;

is_linux && {
    sudo ~/workstation/bin/enable-passwordless-sudo.sh
    sudo apt-get update
}

# [[file:workstation.org::nix_darwin_rebuild_flake_function][nix_darwin_rebuild_flake_function]]
function nix_darwin_rebuild_flake() {
    nix build --extra-experimental-features "nix-command flakes" \
        ~/workstation\#darwinConfigurations.${WORKSTATION_NAME}.system
    ./result/sw/bin/darwin-rebuild switch --flake ~/workstation#${WORKSTATION_NAME}

    rm -rf ./result
}
# nix_darwin_rebuild_flake_function ends here

is_mac && {
    info installing darwin-nix
    cd $WS_DIR
    nix-build https://github.com/LnL7/nix-darwin/archive/${NIX_DARWIN_VERSION}.tar.gz -A installer
    ./result/bin/darwin-installer

    nix_darwin_rebuild_flake

    info finished installing darwin-nix
}

export NIX_PATH=""
export HOME_MANAGER_BACKUP_EXT=old

nix run home-manager/$HOME_MANAGER_VERSION -- init ~/workstation

# [[file:workstation.org::home_manager_flake_switch_function][home_manager_flake_switch_function]]
function home_manager_flake_switch() {
    nix build --no-link ~/workstation/#homeConfigurations.${WORKSTATION_NAME}.$(whoami).activationPackage --show-trace
    "$(nix path-info ~/workstation/#homeConfigurations.${WORKSTATION_NAME}.$(whoami).activationPackage)"/activate --show-trace
}
# home_manager_flake_switch_function ends here
home_manager_flake_switch

set +u
# evaluating this with set -u will cause an unbound variable error
source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
set -u

install_doom_emacs_no_nix

echo "building the 'ws' script"
cd  ~/workstation/wshs
nix build --no-link -L .#"wshs:exe:bww" .#"wshs:exe:ws"
echo "running the 'ws install' process"
$(nix path-info .#"wshs:exe:ws")/bin/ws install -m "$WORKSTATION_NAME";
echo "'ws install' process completed"

info linking dotfiles that should be symlinked
chezmoi -c ~/workstation/dotfiles/dot_config/chezmoi/chezmoi.toml --source ~/workstation/dotfiles apply
info finished linking dotfiles

set +e
echo "Running final installs (install)"
if is_linux; then
    echo "is linux, installing ripgrep, fdfind, etc via apt";
    time sudo apt-get install ripgrep fd-find zsh make libtool libvterm-dev;
    echo "done running final installs";
else
    echo "linux not detected, no final installs necessary";
fi

# why is bash so cryptic
if [ ! -z "${BW_CLIENTID+x}" ] && \
   [ ! -z "${BW_CLIENTSECRET+x}" ] && \
   [ ! -z "${WS_BW_MASTER_PASS+x}" ]; then
    info variables requried to run bww force-sync are set, running
    if [ ! -d ~/secrets ]; then
        mkdir ~/secrets;
    fi
    # overwriting anything that was previously in the file
    echo "${WS_BW_MASTER_PASS}" > ~/secrets/bw_pass
    bw login --apikey
    bw_unlock
    bw sync
    $(nix path-info .#"wshs:exe:bww")/bin/bww force-sync
else
    info variables required to run bww force sync are MISSING, skipping
fi

cat <<-EOF
Success! However, there are some remaining manual set up steps required.
# [[file:workstation.org::manual-setup-instructions][manual-setup-instructions]]
There are unfortunately a number of things need to install and set up
manually:
- lastpass firefox extension
- vimium-ff etension
- dropbox
- icloud
- slack
- spotify
- install haskell language server in ~/bin (or somwewhere else?) for hls

These are the settings I use for slack:
- accessibility then at bottom changbe up arrow to move focus to last message
- advanced
  - when in markdown block backticks, enter should do a newline
  - format messages with markup

mac settings
- enable screen sharing, _not_ remote management
- enable remote login
- configure hammerspoon
  - open it
  - enable accessability settings
  - launch at login
# manual-setup-instructions ends here
EOF
# Bootstraping Script:1 ends here
