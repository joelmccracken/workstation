#!/usr/bin/env sh
#
cd ~/Dropbox/EF
git add .
git commit -m "$(date)"
git fetch
git reset --hard origin/main
~/.emacs.d/bin/doomscript ~/workstation/bin/org-mobile-push.el
