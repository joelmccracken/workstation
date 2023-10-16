#!/usr/bin/env bash
# Polite git checkout
# This script provides a way to check out a repository in a directory without
# clobbering the existing contents of the directory. This is useful in case the
# directory might have contents that you wish to save, and you think it might be
# handy to be able to i.e. ~git diff~ the contents against what git knows about
# in the repository, once all of the trivial differences have been resolved (i.e.
# files missing are put into place).

# I used to use this for setting up dotfiles, however, I've changed the approach,
# but I still think this script is handy and want to hang on to it.

# [[file:../workstation.org::*Polite git checkout][Polite git checkout:1]]
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
polite-git-checkout $1 $2
# Polite git checkout:1 ends here
