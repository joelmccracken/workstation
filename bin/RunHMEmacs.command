#!/usr/bin/env bash

# opens emacs installed w home manager; it is easier/faster to do it with this than clicking like 5
# buttons to navigate to the correct place to open it. doing that just distracts me from whatever I
# was trying to accomplish in the first place.
# this script can't be dragged onto the dock for even easier access, but it can be put in launchpad.

osascript - <<EOF &
tell application "/Users/joel/Applications/Home Manager Apps/Emacs.app/Contents/MacOS/Emacs"
	activate
end tell

delay 1

tell application id "com.apple.Terminal" to close the front window
EOF
