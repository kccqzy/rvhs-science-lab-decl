#!/bin/bash
set -e
set -x

# Create docker machine and start it.
if ! docker-machine ls -q | grep '^labdecl-staging$'; then

    docker-machine create --driver google \
                   --google-project rvhs-sci-lab-undertaking \
                   --google-zone asia-east1-a \
                   --google-machine-type f1-micro \
                   --google-address labdecl-staging-ip \
                   --google-machine-image https://www.googleapis.com/compute/v1/projects/ubuntu-os-cloud/global/images/ubuntu-1404-trusty-v20151113 \
                   labdecl-staging

    # Get HTTPS Certificates. This step requires manual intervention! First the
    # firewall setting needs to be changed to allow 80 and 443 traffic. Then a
    # human needs to enter domain names that correspond to the current IP
    # address of the instance.
    eval $(docker-machine env labdecl-staging)
    docker run -it --rm -p 443:443 -p 80:80 \
           --name letsencrypt \
           -v /etc/letsencrypt:/etc/letsencrypt \
           -v /var/lib/letsencrypt:/var/lib/letsencrypt \
           quay.io/letsencrypt/letsencrypt:latest \
           auth

elif [ "Running" != "$(docker-machine status labdecl-staging)" ]; then
    docker-machine start labdecl-staging
fi

eval $(docker-machine env labdecl-staging)

## Part I: the app.

# Create a context dir.
[ -e ../deploy-context ] && rm -rf ../deploy-context
mkdir ../deploy-context

# Copy the Dockerfile to the context dir.
cp -f deploy-staging/Dockerfile-labdecl ../deploy-context/Dockerfile

# Tar and compress and copy the executable to the context dir.
tar cC .. buildresult | xz -vc9 > ../deploy-context/buildresult.tar.xz

# Start building.
docker build --file=../deploy-context/Dockerfile -t labdecl-app-staging ../deploy-context

# Remove this context folder.
rm -rf ../deploy-context

# Stop and delete the old version of the app.
if docker ps -a --format '{{.Names}}' | grep labdecl-app-staging; then
    if docker ps --format '{{.Names}}' | grep labdecl-app-staging; then
        docker stop labdecl-app-staging
    fi
    docker rm labdecl-app-staging
fi

# Run the app.
docker run -e "APPROOT=https://staging.rvhs.space" \
       --name labdecl-app-staging \
       -v /database:/database \
       --restart=always \
       --log-driver=json-file \
       --log-opt max-size=100m \
       -h staging.rvhs.space \
       -p 8081:8081 \
       -d labdecl-app-staging

## Part II: the proxy.

# Start building.
docker build --file=deploy-staging/Dockerfile-nginx -t labdecl-nginx-staging deploy-staging

# Stop and delete the old version of the proxy.
if docker ps -a --format '{{.Names}}' | grep labdecl-nginx-staging; then
    if docker ps --format '{{.Names}}' | grep labdecl-nginx-staging; then
        docker stop labdecl-nginx-staging
    fi
    docker rm labdecl-nginx-staging
fi

# Run the proxy.
docker run \
       -v /etc/letsencrypt:/etc/letsencrypt \
       --name labdecl-nginx-staging \
       --link labdecl-app-staging \
       --restart=always \
       --log-driver=json-file \
       --log-opt max-size=100m \
       -p 80:80 -p 443:443 \
       -d labdecl-nginx-staging
