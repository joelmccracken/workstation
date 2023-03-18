#!/usr/bin/env bash

function ln_dotfile() {
    ln -s ~/workstation/dotfiles/$1 ~/.$1;
}

function ln_norm() {
    ln -s ~/workstation/dotfiles/$1 ~/$1;
}

ln_dotfile bashrc
ln_dotfile config
ln_dotfile doom.d
ln_dotfile ghci
ln_dotfile gitconfig
ln_dotfile gitignore
ln_dotfile gitignore
ln_dotfile hammerspoon
ln_dotfile nix-channels
ln_dotfile npmrc
ln_dotfile reddup.yml
ln_dotfile zcompcache
ln_dotfile zshrc
ln_dotfile zshrc.aeglos.sh

ln_norm Brewfile
ln_norm Brewfile.lock.json
ln_norm bitbar
ln_norm README.md
