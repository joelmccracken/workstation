#!/usr/bin/env bash
# Bootstraping Script
# This script is intended to be entrypoint to this project. It can be curled to a
# new machine and then run, and will set things up on that machine as necessary.

# [[file:workstation.org::*Bootstraping Script][Bootstraping Script:1]]
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

set -xeuo pipefail

export NIX_PM_VERSION=nix-2.11.1
export NIX_DARWIN_VERSION=f6648ca0698d1611d7eadfa72b122252b833f86c
export HOME_MANAGER_VERSION=213a06295dff96668a1d673b9fd1c03ce1de6745

if [ -z "${1+x}" ]; then
    echo WORKSTATION_NAME must be provided as first argument
    exit 2
else
    export WORKSTATION_NAME="$1"
fi

if [ -z "${2+x}" ]; then
    export WORKSTATION_BOOTSTRAP_COMMIT=origin/master
else
    export WORKSTATION_BOOTSTRAP_COMMIT="$2"
fi

WS_DIR="$HOME/workstation"
export WORKSTATION_HOST_SETTINGS_SRC_DIR=$WS_DIR/hosts/$WORKSTATION_NAME
export WORKSTATION_HOST_CURRENT_SETTINGS_DIR=$WS_DIR/hosts/current

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
    cd $WS_DIR
    [[ "$(git remote get-url origin)" == 'git@github.com:joelmccracken/workstation.git' ]]
} || {
    time mv_dir_dated_backup ~/workstation
    time git clone 'https://github.com/joelmccracken/workstation.git'
    cd workstation
    git checkout $WORKSTATION_BOOTSTRAP_COMMIT
}

echo DEBUG setting current host settings directory...
echo DEBUG workstation host settings directory: $WORKSTATION_HOST_SETTINGS_SRC_DIR

if [ -d $WORKSTATION_HOST_SETTINGS_SRC_DIR ]; then
    echo DEBUG setting current host directory to $WORKSTATION_HOST_SETTINGS_SRC_DIR;
    ln -s $WORKSTATION_HOST_SETTINGS_SRC_DIR $WORKSTATION_HOST_CURRENT_SETTINGS_DIR;
else
    echo ERROR $WORKSTATION_HOST_SETTINGS_SRC_DIR does not exist, must exit
    exit 5
fi

echo DEBUG installing nix

{ which nix > /dev/null; } || {
    time sh <(curl -L https://releases.nixos.org/nix/$NIX_PM_VERSION/install) --daemon;
}

export NIX_REMOTE=daemon
is_linux &&  {
(sudo bash -c 'mkdir -p /etc/nix; cat > /etc/nix/nix.conf') <<-EOF
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
substituters = https://cache.nixos.org https://cache.iog.io
experimental-features = nix-command flakes
trusted-users = root joel runner
build-users-group = nixbld
# END OF /etc/nix/nix.conf
EOF
# cat /etc/nix/nix.conf
    time sudo systemctl restart nix-daemon.service;
}

restart_mac_daemon() {
    set +e
    time sudo launchctl unload /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    time sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    set -e
}

is_mac && {
    restart_mac_daemon
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
    cd $WS_DIR
    nix-build https://github.com/LnL7/nix-darwin/archive/${NIX_DARWIN_VERSION}.tar.gz -A installer
    ./result/bin/darwin-installer

    nix build ~/workstation\#darwinConfigurations.glamdring.system
    ./result/sw/bin/darwin-rebuild switch --flake ~/workstation#glamdring

    rm -rf ./result
}


export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}
nix-channel --add https://github.com/nix-community/home-manager/archive/${HOME_MANAGER_VERSION}.tar.gz home-manager
nix-channel --update
export HOME_MANAGER_BACKUP_EXT=old

# mkdir -p $HOME/.config/nixpkgs/
# ln -s $HOME/workstation/home.nix $HOME/.config/nixpkgs/home.nix
# cat $HOME/.config/nixpkgs/home.nix
nix-shell '<home-manager>' -A install

set +u
# evaluating this with set -u will cause an unbound variable error
source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
set -u


# home-manager switch -f ~/workstation/home.nix

nix build --no-link ~/workstation/#homeConfigurations.${WORKSTATION_NAME}.$(whoami).activationPackage
"$(nix path-info ~/workstation/#homeConfigurations.${WORKSTATION_NAME}.$(whoami).activationPackage)"/activate

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

# Bootstraping Script:1 ends here
