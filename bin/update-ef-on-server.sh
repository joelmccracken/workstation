#!/usr/bin/env sh
#
cd ~/EF
git diff
git add .
git commit -m "$(date)"
git fetch
git reset --hard origin/main
~/.config/emacs/bin/doomscript ~/workstation/bin/org-mobile-push.el
git diff
git add .
git commit -m "$(date)"
git push origin main
