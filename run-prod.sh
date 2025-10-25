#!/bin/sh

app_env="${APP_ENV:-production}"
APP_ENV="$app_env" screen -UdmS booker sbcl --core booker.image --no-userinit
