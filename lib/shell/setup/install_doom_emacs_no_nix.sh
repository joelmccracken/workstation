#!/usr/bin/env bash
# [[file:../../../workstation.org::*Install doom emacs without nix][Install doom emacs without nix:2]]
source ~/workstation/lib/shell/foundation.sh

function install_doom_emacs_no_nix() {
    {
        cd $WORKSTATION_EMACS_CONFIG_DIR
        [[ "$(git remote get-url origin)" == 'https://github.com/hlissner/doom-emacs' ]]
    } || {
        mv_dated_backup $WORKSTATION_EMACS_CONFIG_DIR
        time git clone --depth 1 https://github.com/doomemacs/doomemacs $WORKSTATION_EMACS_CONFIG_DIR/
        # alternative: use this if encounter problems
        # ~/.emacs.d/bin/doom -y install;
        # time timeout 45m bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0
        # time bash -c 'yes | ~/.emacs.d/bin/doom install' || exit 0
        time timeout 60m bash -c "yes | $WORKSTATION_EMACS_CONFIG_DIR/bin/doom install" || exit 0
        $WORKSTATION_EMACS_CONFIG_DIR/bin/doom sync
        echo FINISHED INSTALLING DOOM;
    }
}
install_doom_emacs_no_nix
# Install doom emacs without nix:2 ends here
