#!/bin/sh

app_env="${APP_ENV:-production}"
APP_ENV="$app_env" screen -UdmS booker /bin/sh -c 'sbcl --core booker.image --no-userinit; exec sh'
