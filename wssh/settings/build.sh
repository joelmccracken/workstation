# TODO move this to an org mode file
export PKG_CONFIG_PATH="/usr/local/opt/openssl/lib/pkgconfig:${PKG_CONFIG_PATH}"
export LDFLAGS="-L/usr/local/opt/libpq/lib"
export CPPFLAGS="-I/usr/local/opt/libpq/include"
export PKG_CONFIG_PATH="/usr/local/opt/libpq/lib/pkgconfig"
export PG9DIR=$HOME/postgres9-everything-not-firehose/data
export PG11DIR=$HOME/postgres9-firehose/data
export PG11PORT=5433
export PGUSER=postgres
