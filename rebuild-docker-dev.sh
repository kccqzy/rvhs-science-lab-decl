#!/bin/bash
set -e
set -x

export http_proxy=
export https_proxy=
export no_proxy=
export HTTP_PROXY=
export HTTPS_PROXY=
export NO_PROXY=

if ! docker-machine ls | grep '^labdecl-buildbot\b'; then
    docker-machine create --driver vmwarefusion labdecl-buildbot
elif [ "Running" != "$(docker-machine status labdecl-buildbot)" ]; then
    docker-machine start labdecl-buildbot
fi

NAME=$(uuidgen | tr A-Z a-z)
eval $(docker-machine env labdecl-buildbot)
docker build --memory=2g --file=./Dockerfile -t "$NAME" .
APPROOT="http://$(docker-machine ip labdecl-buildbot):8081"
docker run -it --rm -v /Users/chamekim/Documents/Schoolwork/Post-RVHS/Lab-Declaration/buildresult:/buildresult-host "$NAME" cp -f /buildresult/labdecl /buildresult-host
docker run -it -e "APPROOT=$APPROOT" --rm -p 8081:8081 -v /Users/chamekim/Documents/Schoolwork/Post-RVHS/Lab-Declaration/src/state:/buildhome/state "$NAME"
