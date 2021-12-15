#!/bin/bash

echo LOCAL_UID=$(id -u $USER) > .env
echo LOCAL_GID=$(id -g $USER) >> .env