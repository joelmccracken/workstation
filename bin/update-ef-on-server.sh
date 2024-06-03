#!/usr/bin/env bash
cd ~/EF
if [ -f ./autosync-settings.sh ]; then
    source ./autosync-settings.sh
fi

runEFUpdate() {
    echo starting at $(git rev-parse HEAD)
    git diff
    git add .
    git commit -m "$(date)"
    git fetch
    # git reset --hard origin/main
    git rebase origin/main
    ~/.config/emacs/bin/doomscript ~/workstation/bin/org-mobile-push.el
    git diff
    git add .
    git commit -m "$(date)"
    git push origin main
}

if [ "${WORKSTATION_AUTORUN:-false}" = "true" ]; then
  if [ "${EF_AUTOSYNC:-false}" = "true" ]; then
    runEFUpdate ;
  else
      echo "Autosync disabled, not updating EF"
  fi
else
  runEFUpdate;
fi
