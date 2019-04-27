#!/bin/sh
set -e

# allow the container to be started with `--user`
if [ "$1" = /bloberl* -a "$(id -u)" = '0' ]; then
	find . \! -user bloberl -exec chown bloberl '{}' +
	exec su-exec bloberl "$0" "$@"
fi

exec "$@"