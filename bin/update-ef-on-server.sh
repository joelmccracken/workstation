#!/usr/bin/env sh
#
cd ~/EF
git add .
git commit -m "$(date)"
git fetch
git reset --hard origin/main
~/.config/emacs/bin/doomscript ~/workstation/bin/org-mobile-push.el
git add .
git commit -m "$(date)"
git push origin main
