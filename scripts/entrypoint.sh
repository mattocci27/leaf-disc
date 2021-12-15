#!/bin/bash

USER_ID=${LOCAL_UID:-9001}
GROUP_ID=${LOCAL_GID:-9001}

echo "Starting with UID : $USER_ID, GID: $GROUP_ID"
useradd -u $USER_ID -o -m app
groupmod -g $GROUP_ID app
export HOME=/home/app

exec /usr/sbin/gosu app "$@"