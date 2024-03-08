#!/usr/bin/env bash
# Build WS tool

# [[file:../../../workstation.org::*Build WS tool][Build WS tool:1]]
cd  ~/workstation/wshs
nix build --no-link -L .#"wshs:exe:bww" .#"wshs:exe:ws"
# Build WS tool:1 ends here
