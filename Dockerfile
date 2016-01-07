FROM ubuntu:14.04
MAINTAINER QZY <qzy@qzy.io>
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
RUN echo 'deb http://download.fpcomplete.com/ubuntu trusty main' | tee /etc/apt/sources.list.d/fpco.list
RUN apt-get update && apt-get install -y stack libicu-dev && apt-get clean && rm -rf /var/lib/apt/lists/*
ADD ./texlive-2014-portable.tar.xz /
RUN mkdir /tmp/latex-run /buildhome
WORKDIR /buildhome
COPY LICENSE Setup.hs stack.yaml labdecl.cabal /buildhome/
RUN stack setup
RUN stack install alex happy
RUN stack build --only-dependencies --prefetch
COPY tex-report.tar.gz Main.hs /buildhome/
COPY LabDecl /buildhome/LabDecl
COPY static /buildhome/static
COPY templates /buildhome/templates
RUN mkdir /buildresult && stack build --copy-bins --local-bin-path /buildresult
ENV LATEX_RUN_FOLDER=/tmp/latex-run GOOGLE_CLIENT_ID=NULL GOOGLE_CLIENT_SECRET=NULL LUALATEX=/texlive-2014-portable/bin/x86_64-linux/lualatex LISTEN_PORT=8081 LISTEN_HOST="*4"
EXPOSE 8081
CMD /buildresult/labdecl +RTS -N -qa -A8m -sstderr
STOPSIGNAL SIGINT
