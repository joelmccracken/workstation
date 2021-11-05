export PATH="$HOME/.cabal/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.rbenv/shims:$PATH"

export PATH=$HOME/.local/bin:$PATH
export PATH="/usr/local/opt/libpq/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PKG_CONFIG_PATH="/usr/local/opt/openssl/lib/pkgconfig:${PKG_CONFIG_PATH}"

export PATH="/usr/local/opt/libpq/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/libpq/lib"
export CPPFLAGS="-I/usr/local/opt/libpq/include"
export PKG_CONFIG_PATH="/usr/local/opt/libpq/lib/pkgconfig"

export PG9DIR=$HOME/postgres9-everything-not-firehose/data
export PG11DIR=$HOME/postgres9-firehose/data
export PG11PORT=5433
export PGUSER=postgres

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

bwAuth () {
    # authtenticates bitwarden for this shell session only
    export BW_SESSION=`bw unlock --passwordfile ~/secrets/bw_pass --raw`;
}

sourceIfExists () {
    if [ -f "$1" ]; then
        source "$1"
    fi
}

sourceIfExists ~/.nix-profile/etc/profile.d/nix.sh
sourceIfExists ~/etc/machine-settings.sh
sourceIfExists ~/.zshrc.${WORKSTATION_NAME}.sh
if [ -e /Users/joel/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/joel/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
