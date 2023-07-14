#!/usr/bin/env bash

# (*
# tell application "/nix/store/iiwr9qm4xib31jk596d7ch4x2dhjg32p-emacs-with-packages-with-packages-28.2/Applications/Emacs.app/Contents/MacOS/Emacs"
# 	activate
# end tell
# *)


# (* doesn't work:
# error "Can’t get application \"~/Applications/Home\\\\ Manager\\\\ Apps/Emacs.app/Contents/MacOS/Emacs\"." number -1728 from current application

# tell application "~/Applications/Home Manager Apps/Emacs.app/Contents/MacOS/Emacs"
# 	activate
# end tell
# *)

osascript - <<EOF &

tell application "/Users/joel/Applications/Home Manager Apps/Emacs.app/Contents/MacOS/Emacs"
	activate
end tell

delay 1

tell application id "com.apple.Terminal" to close the front window
EOF
