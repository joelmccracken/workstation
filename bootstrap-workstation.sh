#!/usr/bin/env bash
# WARNING: This file is managed by tangling workstation.org. Do not edit directly!

set -xeuo pipefail

if [ -z "${1+x}" ]; then
    WORKSTATION_BOOTSTRAP_COMMIT=master
else
    WORKSTATION_BOOTSTRAP_COMMIT="$1"
fi

WS_DIR="$HOME/workstation"

function is_mac() {
    [[ "$(uname)" == 'Darwin' ]]
}

function is_linux() {
    [[ "$(uname)" == 'Linux' ]]
}

is_mac && {
    sudo bash -c '(xcodebuild -license accept; xcode-select --install) || exit 0'

    which brew > /dev/null || {
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

        # install git, necessary for next step
        # TODO this should be safe to run even if its been run before
        brew install git
    }
}

is_linux && {
    sudo bash -c 'apt-get update && apt-get install git'
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
} || polite-git-checkout ~ 'https://github.com/joelmccracken/dotfiles.git'

{ cd ~/worksation
     [[ "$(git remote get-url origin)" == 'git@github.com:joelmccracken/workstation.git' ]]
} || {
    mv_dir_dated_backup ~/workstation
    git clone 'https://github.com/joelmccracken/workstation.git'
}

echo installing nix

{ which nix > /dev/null; } || { sh <(curl -L https://nixos.org/nix/install) --daemon; }

NIX_DAEMON_PATH='/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
cat $NIX_DAEMON_PATH
if [[ -e "$NIX_DAEMON_PATH" ]]; then
    echo "IT WILL BE DONE"
    set +u
    source "$NIX_DAEMON_PATH";
    set -u
fi;

echo "IT HAS BEEN DONE"

# # Nix
# if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
#   . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
# fi
# # End Nix

# for flakes
nix-env -iA nixpkgs.nixUnstable
mkdir -p ~/.config/nix
# echo -e "\nexperimental-features = nix-command flakes\n"

cat > ~/.config/nix/nix.conf <<-EOF
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
substituters = https://cache.nixos.org https://miso-haskell.cachix.org https://hydra.iohk.io
experimental-features = nix-command flakes
EOF


ls /etc/nix
cat /etc/nix/nix.conf

cat ~/.config/nix/nix.conf

echo "POO POO FOR POO POO PUFFS"

# { which stack > /dev/null; } || { sh <(curl -sSL https://get.haskellstack.org/); }
cd  ~/workstation/propellor/
nix build
result/bin/propellor-config



# most of the stuff below this can be moved to the haskell stuff
#
# is_mac && brew bundle
is_linux && {
    sudo ~/workstation/bin/enable-passwordless-sudo.sh
    sudo apt-get update
    sudo snap install emacs --classic
    sudo apt-get install ripgrep fd-find zsh make libtool libvterm-dev
}

{
    cd ~/.emacs.d;
    [[ "$(git remote get-url origin)" == 'https://github.com/hlissner/doom-emacs' ]]
} || {
    mv_dir_dated_backup ~/.emacs.d;
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d;
    # If I ever have issues w/ this, I can use this form:
    # timeout 10m bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0
    # ~/.emacs.d/bin/doom -y env;
    # ~/.emacs.d/bin/doom -y install;
    timeout 10m bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0
    echo FINISHED INSTALLING DOOM;
}
