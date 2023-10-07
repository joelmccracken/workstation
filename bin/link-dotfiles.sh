#!/usr/bin/env bash

source ~/workstation/lib/shell/funcs.sh

FORCE=false;
if [ "$1" == "-f" ]; then
    export FORCE=true
fi

function handle_force() {
    if [ "$FORCE" == "true" ]; then
        mv_dated_backup "$1"
    fi
}

function ln_helper() {
    dest=~/$2$1
    src=~/workstation/dotfiles/$1
    # curr=$(readlink -f "$dest")
    handle_force $dest

    if [ ! -L $dest ] && [ ! -f $dest ]; then
        ln -s "$src" "$dest";
    else
        if [ -f $dest ]; then
            echo warning: file already exists at $dest
        else
            echo warning: symlink already exists at $dest
        fi
    fi
}

function ln_dotfile() {
    ln_helper $1 "."
}

function ln_norm() {
    ln_helper $1 ""
}

function ln_dotfile_n() {
    src=~/workstation/dotfiles/$1
    dest=~/.$1
    destdir=$(dirname dest)

    if [ ! -d $destdir ]; then
        mkdir -p $destdir
    fi

    handle_force $dest

    if [ ! -L $dest ] && [ ! -f $dest ]; then
        ln -s $src $dest;
    else
        if [ -f $dest ]; then
            echo warning: file already exists at $dest.
        fi
    fi
}

ln_dotfile bashrc
ln_dotfile ghci
ln_dotfile gitconfig
ln_dotfile hammerspoon
ln_dotfile nix-channels
ln_dotfile npmrc
ln_dotfile reddup.yml
ln_dotfile zshrc
ln_dotfile zshrc.aeglos.sh
ln_dotfile doom.d

ln_norm Brewfile
ln_norm Brewfile.lock.json
ln_norm bitbar

ln_dotfile_n config/git
