#!/usr/bin/env bash

export PKG_CONFIG_PATH="/usr/local/opt/openssl/lib/pkgconfig:${PKG_CONFIG_PATH}"
export LDFLAGS="-L/usr/local/opt/libpq/lib"
export CPPFLAGS="-I/usr/local/opt/libpq/include"
export PKG_CONFIG_PATH="/usr/local/opt/libpq/lib/pkgconfig"
