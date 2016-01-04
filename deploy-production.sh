#!/bin/bash
set -e
set -x

# Create docker machine and start it.
if ! docker-machine ls | grep '^labdecl-production\b'; then

    docker-machine create --driver google \
                   --google-project rvhs-sci-lab-undertaking \
                   --google-zone asia-east1-a \
                   --google-machine-type g1-small \
                   --google-address labdecl-production-ip \
                   --google-machine-image https://www.googleapis.com/compute/v1/projects/ubuntu-os-cloud/global/images/ubuntu-1404-trusty-v20151113 \
                   labdecl-production

    # Get HTTPS Certificates. This step requires manual intervention! First the
    # firewall setting needs to be changed to allow 80 and 443 traffic. Then a
    # human needs to enter domain names that correspond to the current IP
    # address of the instance.
    eval $(docker-machine env labdecl-production)
    docker run -it --rm -p 443:443 -p 80:80 \
           --name letsencrypt \
           -v /etc/letsencrypt:/etc/letsencrypt \
           -v /var/lib/letsencrypt:/var/lib/letsencrypt \
           quay.io/letsencrypt/letsencrypt:latest \
           auth

elif [ "Running" != "$(docker-machine status labdecl-production)" ]; then
    docker-machine start labdecl-production
fi

eval $(docker-machine env labdecl-production)

## Part I: the app.

# Create a context dir.
[ -e ../deploy-context ] && rm -rf ../deploy-context
mkdir ../deploy-context

# Copy the Dockerfile to the context dir.
cp -f deploy-production/Dockerfile-labdecl ../deploy-context/Dockerfile

# Tar and compress and copy the executable to the context dir.
tar cC .. buildresult | xz -vc9 > ../deploy-context/buildresult.tar.xz

# Start building.
docker build --file=../deploy-context/Dockerfile -t labdecl-app-production ../deploy-context

# Remove this context folder.
rm -rf ../deploy-context

# Stop and delete the old version of the app.
if docker ps -a --format '{{.Names}}' | grep labdecl-app-production; then
    if docker ps --format '{{.Names}}' | grep labdecl-app-production; then
        docker stop labdecl-app-production
    fi
    docker rm labdecl-app-production
fi

# Run the app.
docker run -e "APPROOT=https://sciencelab.rvhs.space" \
       --name labdecl-app-production \
       -v /database:/database \
       --restart=always \
       --log-driver=json-file \
       --log-opt max-size=100m \
       -h sciencelab.rvhs.space \
       -p 8081:8081 \
       -d labdecl-app-production

## Part II: the proxy.

# Start building.
docker build --file=deploy-production/Dockerfile-nginx -t labdecl-nginx-production deploy-production

# Stop and delete the old version of the proxy.
if docker ps -a --format '{{.Names}}' | grep labdecl-nginx-production; then
    if docker ps --format '{{.Names}}' | grep labdecl-nginx-production; then
        docker stop labdecl-nginx-production
    fi
    docker rm labdecl-nginx-production
fi

# Run the proxy.
docker run \
       -v /etc/letsencrypt:/etc/letsencrypt \
       --name labdecl-nginx-production \
       --link labdecl-app-production \
       --restart=always \
       --log-driver=json-file \
       --log-opt max-size=100m \
       -p 80:80 -p 443:443 \
       -d labdecl-nginx-production
