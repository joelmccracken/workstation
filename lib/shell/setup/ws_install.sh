#!/usr/bin/env bash
# Run WS install

# [[file:../../../workstation.org::*Run WS install][Run WS install:1]]
cd  ~/workstation/wshs
$(nix path-info .#"wshs:exe:ws")/bin/ws install -m "$WORKSTATION_NAME";
# Run WS install:1 ends here
