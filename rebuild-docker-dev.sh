#!/bin/bash
set -e
set -x

# Create docker machine and start it.
if ! docker-machine ls | grep '^labdecl-buildbot\b'; then
    docker-machine create --driver vmwarefusion labdecl-buildbot
elif [ "Running" != "$(docker-machine status labdecl-buildbot)" ]; then
    docker-machine start labdecl-buildbot
fi

eval $(docker-machine env labdecl-buildbot)

# Build the app.
docker build --memory=2g --file=./Dockerfile -t labdecl-build .

APPROOT="http://$(docker-machine ip labdecl-buildbot):8081"

# Remove all previous build results.
rm -f ../buildresult/*

# Copy the executable from container to ../buildresult
docker run --rm \
       -v /Users/chamekim/Documents/Schoolwork/Post-RVHS/Lab-Declaration/buildresult:/buildresult-host \
       labdecl-build \
       cp -f /buildresult/labdecl /buildresult-host

# Run the development build interactively.
docker run -it \
       -e "DEVELOPMENT=1" -e "APPROOT=$APPROOT" \
       --rm -p 8081:8081 \
       -v /Users/chamekim/Documents/Schoolwork/Post-RVHS/Lab-Declaration/src/state:/buildhome/state \
       labdecl-build
