#!/usr/bin/env bash

cd ~/workstation
git add .
# git stash
git fetch origin
git rebase --autostash origin/master

# git stash pop
