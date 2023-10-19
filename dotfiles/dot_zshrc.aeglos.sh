#!/usr/bin/env bash

sourceIfExists () {
    if [ -f "$1" ]; then
        source "$1"
    fi
}

alias python3=~/.pyenv/versions/3.6.8/bin/python3

sourceIfExists ~/secrets/tvision_keys
