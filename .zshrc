# authtenticates bitwarden for this shell session only
bwAuth () {
    export BW_SESSION=`bw unlock --passwordfile ~/secrets/bw_pass --raw`;
}

export PATH="$HOME/.cabal/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.rbenv/shims:$PATH"
eval "$(rbenv init -)"

export EDITOR=emacsclient
export GIT_EDITOR=$EDITOR

alias lock=/System/Library/Frameworks/ScreenSaver.framework/Versions/Current/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine

export LANG=en_US.UTF-8

# TODO figure out how to make this work in zsh
# shopt -s extglob

export HISTCONTROL=erasedups
export HISTSIZE=10000

# TODO figure out how to make this work in zsh
# shopt -s histappend

do_command_done_alert() {
    osascript -e 'display dialog "Command Done!"'
}

alert_when_done() {
    if test -n "$1";
    then
        while kill -0 $1
        do
            sleep 1
        done
    fi
    do_command_done_alert
}
if [ -e /Users/joel/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/joel/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
