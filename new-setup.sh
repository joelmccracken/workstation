#!/usr/bin/env bash
set -euo pipefail

echo $(pwd)

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew bundle

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
yes | ~/.emacs.d/bin/doom install
# ~/.emacs.d/bin/doom
# ~/.emacs.d/bin/doom sync
