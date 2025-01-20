#!/usr/bin/env bash
export BASH_SILENCE_DEPRECATION_WARNING=1

sourceIfExists () {
    if [ -f "$1" ]; then
        source "$1"
    fi
}
if [[ -x "/opt/homebrew/bin/brew" ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

export WORKSTATION_DIR="${WORKSTATION_DIR:-~/workstation}"

sourceIfExists ${WORKSTATION_DIR}/lib/shell/settings.sh
sourceIfExists ${WORKSTATION_DIR}/lib/shell/paths.sh
sourceIfExists ${WORKSTATION_DIR}/hosts/current/zshrc.sh


export EDITOR=emacsclient
export GIT_EDITOR=$EDITOR

export LANG=en_US.UTF-8

export HISTCONTROL=erasedups
export HISTSIZE=10000
export HISTCONTROL=ignoreboth
export HISTFILESIZE=2000

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
export C_INCLUDE_PATH
C_INCLUDE_PATH="$(xcrun --show-sdk-path)/usr/include/ffi:/opt/homebrew/Cellar/pcre/8.45/include:$C_INCLUDE_PATH"
C_INCLUDE_PATH="/opt/homebrew/Cellar/librdkafka/2.3.0/include:$C_INCLUDE_PATH"
C_INCLUDE_PATH="/opt/homebrew/Cellar/zstd/1.5.5/include:$C_INCLUDE_PATH"
C_INCLUDE_PATH="/opt/homebrew/Cellar/openssl@3/3.2.1/include/openssl:$C_INCLUDE_PATH"
C_INCLUDE_PATH="/opt/homebrew/Cellar/mysql-client@8.0/8.0.36/include/mysql-client:$C_INCLUDE_PATH"
# export C_INCLUDE_PATH="/opt/homebrew/opt/mysql-client/include:$C_INCLUDE_PATH"

export C_LIBRARY_PATH="/opt/homebrew/Cellar/librdkafka/2.3.0/lib:$C_LIBRARY_PATH"
export C_LIBRARY_PATH="/opt/homebrew/Cellar/zstd/1.5.5/lib:$C_LIBRARY_PATH"
export C_LIBRARY_PATH="/opt/homebrew/Cellar/openssl@3/3.2.1/lib:$C_LIBRARY_PATH"
export C_LIBRARY_PATH="/opt/homebrew/Cellar/mysql-client@8.0/8.0.36/lib:$C_LIBRARY_PATH"
# export C_LIBRARY_PATH="/opt/homebrew/opt/mysql-client/lib:$C_LIBRARY_PATH"


export INCLUDE_PATH="$C_INCLUDE_PATH:$INCLUDE_PATH"
export LIBRARY_PATH="$C_LIBRARY_PATH:$LIBRARY_PATH"
export LD_LIBRARY_PATH="$C_LIBRARY_PATH:$LD_LIBRARY_PATH"

export NVM_DIR="$HOME/.nvm"

[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/opt/homebrew/Caskroom/miniforge/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/opt/homebrew/Caskroom/miniforge/base/etc/profile.d/conda.sh" ]; then
#         . "/opt/homebrew/Caskroom/miniforge/base/etc/profile.d/conda.sh"
#     else
#         export PATH="/opt/homebrew/Caskroom/miniforge/base/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# # <<< conda initialize <<<



# To use the bundled libc++ please add the following LDFLAGS:
#   LDFLAGS="-L/opt/homebrew/opt/llvm@13/lib/c++ -Wl,-rpath,/opt/homebrew/opt/llvm@13/lib/c++"

# llvm@13 is keg-only, which means it was not symlinked into /opt/homebrew,
# because this is an alternate version of another formula.

export PATH="/opt/homebrew/opt/llvm@13/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/llvm@13/lib -L/opt/homebrew/opt/mysql-client@8.0/lib -L/opt/homebrew/opt/libffi/lib"
export CPPFLAGS="-I/opt/homebrew/opt/llvm@13/include -I/opt/homebrew/opt/mysql-client@8.0/include -I/opt/homebrew/opt/libffi/include"
export PKG_CONFIG_PATH="/opt/homebrew/opt/mysql-client@8.0/lib/pkgconfig"

export PATH="$HOME/.local/bin:$PATH"
export MANPATH="$HOME/.local/share/man:$MANPATH"

export HISTSIZE=1000
export SAVEHIST=10000
export CFLAGS="-arch arm64 ${CFLAGS:-}"

if [[ -n "$BASH_VERSION" ]]; then
    # bash specific settings
    shopt -s checkwinsize
    shopt -s histappend
fi

[ -f "/Users/joel.mccraken/.ghcup/env" ] && . "/Users/joel.mccraken/.ghcup/env" # ghcup-env

# export WORKSTATION_DIR=~/workstation2
# source ~/workstation2/dotfiles/zshrc
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export COLUMNS="120"

export PATH="$PATH:/Users/joel.mccraken/.config/ACLI"

. ~/freckle/jnm/bin/freckle.bash


# added by Snowflake SnowSQL installer v1.2
export PATH=/Applications/SnowSQL.app/Contents/MacOS:$PATH

export COLUMNS="120"

export PATH="$PATH:/Users/joel.mccraken/.config/ACLI"


export FR_DOCKERHOST=host.docker.internal

sourceIfExists /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
sourceIfExists ~/.nix-profile/etc/profile.d/hm-session-vars.sh
sourceIfExists ~/workstation/lib/shell/funcs.sh
sourceIfExists ~/.ghcup/env

sourceIfExists ${WORKSTATION_DIR}/lib/shell/funcs.sh
