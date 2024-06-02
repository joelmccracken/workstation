#!/usr/bin/env bash

if [ "$(git status -s | wc -l)" -gt 0 ]; then
    git add .
    git commit -m "autocommit - $(date) - $WORKSTATION_NAME"
fi
