#!/usr/bin/env bash
# Bootstraping Script
# This script is intended to be entrypoint to this project. It can be curled to a
# new machine and then run, and will set things up on that machine as necessary.

# [[file:workstation.org::*Bootstraping Script][Bootstraping Script:1]]
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

set -xeuo pipefail

export NIX_DARWIN_VERSION=02d2551c927b7d65ded1b3c7cd13da5cc7ae3fcf

if [ -z "${1+x}" ]; then
    echo WORKSTATION_NAME must be provided as first argument
    exit 2
else
    WORKSTATION_NAME="$1"
fi

if [ -z "${2+x}" ]; then
    WORKSTATION_BOOTSTRAP_COMMIT=origin/master
else
    WORKSTATION_BOOTSTRAP_COMMIT="$2"
fi

WS_DIR="$HOME/workstation"

function is_mac() {
    [[ "$(uname)" == 'Darwin' ]]
}

function is_linux() {
    [[ "$(uname)" == 'Linux' ]]
}

is_mac && {
    sudo time bash -c '(xcodebuild -license accept; xcode-select --install) || exit 0'

    which brew > /dev/null || {
        time /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

        # install git, necessary for next step
        # TODO this should be safe to run even if its been run before
        time brew install git
    }
}

is_linux && {
    time sudo bash -c 'apt-get update && apt-get install git'
}

polite-git-checkout () {
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

function mv_dir_dated_backup() {
    local THEDIR="$1"
    if test -e "$THEDIR"; then
        mv "$THEDIR" "${THEDIR}-$(date +"%s")"
    fi
}

{
    cd ~;
    [[ "$(git remote get-url origin)" == 'git@github.com:joelmccracken/dotfiles.git' ]]
} || time polite-git-checkout ~ 'https://github.com/joelmccracken/dotfiles.git'

{
    cd ~/worksation;
    [[ "$(git remote get-url origin)" == 'git@github.com:joelmccracken/workstation.git' ]]
} || {
    time mv_dir_dated_backup ~/workstation
    time git clone 'https://github.com/joelmccracken/workstation.git'
    cd workstation
    git checkout $WORKSTATION_BOOTSTRAP_COMMIT
}

echo installing nix

{ which nix > /dev/null; } || {
    time sh <(curl -L https://releases.nixos.org/nix/nix-2.5.1/install) --daemon;
}

export NIX_REMOTE=daemon
is_linux &&  {
(sudo bash -c 'mkdir -p /etc/nix; cat > /etc/nix/nix.conf') <<-EOF
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
substituters = https://cache.nixos.org https://hydra.iohk.io
experimental-features = nix-command flakes
trusted-users = root joel runner
build-users-group = nixbld
# END OF /etc/nix/nix.conf
EOF
# cat /etc/nix/nix.conf
    time sudo systemctl restart nix-daemon.service;
}

is_mac && {
    set +e
    time sudo launchctl unload /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    time sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    set -e
}

NIX_DAEMON_PATH='/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
cat $NIX_DAEMON_PATH
if [[ -e "$NIX_DAEMON_PATH" ]]; then
    set +u
    source "$NIX_DAEMON_PATH";
    set -u
fi;

is_linux && {
    time sudo ~/workstation/bin/enable-passwordless-sudo.sh
    time sudo apt-get update
    # it seems that on github CI snap
    # is not installed:
    # https://github.com/joelmccracken/workstation/runs/5981940574?check_suite_focus=true
    # + ./result/bin/ws install -m ci-ubuntu
    # sudo:  snap: command not found
    # https://phoenixnap.com/kb/install-snap-ubuntu
    # time sudo apt install snapd
    # kinda sorta try to do this from:
    # https://github.com/actions/virtual-environments/issues/2209#issuecomment-740526589
    [ -e /run/user/1001 ] || {
        sudo mkdir /run/user/1001
    }
    sudo chmod -R 777 /run/user/1001

    # just checking to see if the bin is there..
    ls -lah /usr/bin/snap
    # comment this out for now, just to see, because build seemed to pass/execute OK?
    # sudo snap install emacs --classic
    sudo snap list
}

is_mac && {
    sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.old
    cd ~/workstation
    nix-build https://github.com/LnL7/nix-darwin/archive/${NIX_DARWIN_VERSION}.tar.gz -A installer
    ./result/bin/darwin-installer

    source /etc/static/bashrc

    nix build ~/workstation\#darwinConfigurations.glamdring.system
    ./result/sw/bin/darwin-rebuild switch --flake ~/workstation
    rm -rf ./result
}

cd  ~/workstation/wshs

time nix build -L

time ./result/bin/ws install -m "$WORKSTATION_NAME";

# most of the stuff below this can be moved to propellor

is_linux && {
    time sudo apt-get install ripgrep fd-find zsh make libtool libvterm-dev
}

{
    cd ~/.emacs.d;
    [[ "$(git remote get-url origin)" == 'https://github.com/hlissner/doom-emacs' ]]
} || {
    mv_dir_dated_backup ~/.emacs.d;
    time git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
    # alternative: use this if encounter problems
    # ~/.emacs.d/bin/doom -y install;
    # time timeout 45m bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0
    # time bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0
    time timeout 60m bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0
    ~/.emacs.d/bin/doom sync
    echo FINISHED INSTALLING DOOM;
}

HOME_MANAGER_SHA=0304f0f58b4c538ff704c58d53a778b062810ec7
nix-channel --add https://github.com/nix-community/home-manager/archive/${HOME_MANAGER_SHA}.tar.gz home-manager
nix-channel --update

export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}
export HOME_MANAGER_BACKUP_EXT=old
nix-shell '<home-manager>' -A install
source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh

# Bootstraping Script:1 ends here
