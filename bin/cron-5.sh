#!/usr/bin/env bash

main() {
    export WORKSTATION_AUTORUN=true
    echo RUNNING $(date)

    # SHELL=/bin/sh
    # PWD=/home/joel
    # LOGNAME=joel
    # HOME=/home/joel
    # LANG=C.UTF-8
    # SHLVL=1
    # PATH=/usr/bin:/bin
    # _=/usr/bin/env
    # env

    echo "updating EF"
    # TODO this maybe could be better
    # script to generate this file with pointers to specific workstation dirs
    # perhaps have $HOME/.config/workstation/ standard location as well/instead?
    source $HOME/workstation/hosts/current/zshrc.sh
    $HOME/workstation/bin/update-ef-on-server.sh

    echo "FINISHED $(date)"
}

main >> $HOME/workstation/var/log/cron 2>&1
