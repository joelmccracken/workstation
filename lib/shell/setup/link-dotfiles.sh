#!/usr/bin/env bash
# Linking dotfiles

# [[file:../../../workstation.org::*Linking dotfiles][Linking dotfiles:1]]
source ${WORKSTATION_DIR}/lib/shell/funcs.sh

export FORCE=false;
export VERBOSE=false;
export CHECK=false;

function error() {
    printf "$@" >&2
    exit 1
}

function handle_force() {
    if [ "$FORCE" = "true" ]; then
        mv_dated_backup "$1"
    fi
}

function verbose() {
    if [ "$VERBOSE" = "true" ]; then
        echo "$@"
    fi
}

function check () {
    if [ "$CHECK" = "true" ] || [ "$VERBOSE" = "true" ]; then
        echo "$@"
    fi
}

function ln_helper() {
    dest=~/$2$1
    src=${WORKSTATION_DIR}/dotfiles/$1
    curr=$(readlink -f "$dest")

    if [ -L "$dest" ] && [ "$curr" = "$src" ]; then
        check "OK: $dest already points to $src"
    else
        check "NOT OK: $dest does not point to $src"
        if [ "$CHECK" = "true" ] && ! [ "$FORCE" = "true" ]; then
            exit 11
        fi

        handle_force $dest
        ln -s "$src" "$dest"
    fi
}

function ln_dotfile() {
    ln_helper $1 "."
}

function ln_norm() {
    ln_helper $1 ""
}

function ln_dotfile_n() {
    src=${WORKSTATION_DIR}/dotfiles/$1
    dest=~/.$1
    destdir=$(dirname "$dest")

    if [ ! -d $destdir ]; then
        mkdir -p $destdir
    fi

    ln_helper $1 "."
}

while (( $# > 0 )); do
    opt="$1"
    shift

    case $opt in
        -f)
            FORCE=true
            ;;
        -v)
            VERBOSE=true
            ;;
        -c)
            CHECK=true
            ;;
        *)
            error "%s: error, unknown option '%s'" "$0" "$opt"
            exit 1
            ;;
    esac
done

ln_dotfile bashrc
ln_dotfile ghci
ln_dotfile gitconfig
ln_dotfile hammerspoon
ln_dotfile nix-channels
ln_dotfile npmrc
ln_dotfile reddup.yml
ln_dotfile zshrc

ln_norm Brewfile
ln_norm Brewfile.lock.json
ln_norm bitbar

ln_dotfile_n config/git
ln_dotfile_n config/doom
# Linking dotfiles:1 ends here
