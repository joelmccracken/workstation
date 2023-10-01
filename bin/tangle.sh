#!/usr/bin/env sh

EMACS_CONFIG_DIR=~/.config/emacs;

function find_emacs_init() {
  init_file="";
  for x in "$EMACS_CONFIG_DIR/early-init.el" "$EMACS_CONFIG_DIR/init.el"; do
    if [[ -f "$x" ]]; then
      init_file="$x"
      break;
    fi;
  done;
  if [[ "$init_file" = "" ]]; then
    echo "Error: Could not find emacs init file" 1>&2
    exit 43
  else
    echo "$init_file"
  fi
}

emacs_init="$(find_emacs_init)";

emacs -l "$emacs_init" -l lib/emacs/tangle-file.el --batch --tangle
