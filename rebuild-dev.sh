#!/bin/bash
set -e
set -x
export LATEX_RUN_FOLDER=../scratch/LATEX_RUN_FOLDER
export GOOGLE_CLIENT_ID=NULL
export GOOGLE_CLIENT_SECRET=NULL
export LUALATEX=$(which lualatex)
export LISTEN_HOST=127.0.0.1
export LISTEN_PORT=8081
export APPROOT="http://127.0.0.1:8081"
stack build --ghc-options -DDEVELOPMENT
stack exec labdecl
