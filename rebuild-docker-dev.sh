#!/bin/bash
set -e
set -x

if ! docker-machine ls | grep '^labdecl-buildbot\b'; then
    docker-machine create --driver vmwarefusion labdecl-buildbot
elif [ "Running" != "$(docker-machine status labdecl-buildbot)" ]; then
    docker-machine start labdecl-buildbot
fi

NAME="labdecl-build"
eval $(docker-machine env labdecl-buildbot)
docker build --memory=2g --file=./Dockerfile -t "$NAME" .
APPROOT="http://$(docker-machine ip labdecl-buildbot):8081"
docker run -it --rm -v /Users/chamekim/Documents/Schoolwork/Post-RVHS/Lab-Declaration/buildresult:/buildresult-host "$NAME" cp -f /buildresult/labdecl /buildresult-host
rm -f ../buildresult/labdecl.xz
xz -vk9 ../buildresult/labdecl
docker run -it -e "DEVELOPMENT=1" -e "APPROOT=$APPROOT" --rm -p 8081:8081 -v /Users/chamekim/Documents/Schoolwork/Post-RVHS/Lab-Declaration/src/state:/buildhome/state "$NAME"
