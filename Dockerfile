FROM ubuntu:20.04

ARG BUILD_DATE
ARG VCS_REF
LABEL org.label-schema.build-date=$BUILD_DATE \
org.label-schema.name="RStable" \
org.label-schema.description="A docker image with stable versions of R and a bunch of packages used to calculate indicators." \
org.label-schema.license="MIT" \
org.label-schema.url="e.g. https://www.inbo.be/" \
org.label-schema.vcs-ref=$VCS_REF \
org.label-schema.vcs-url="https://github.com/inbo/indicactoren" \
org.label-schema.vendor="Research Institute for Nature and Forest" \
maintainer="Thierry Onkelinx <thierry.onkelinx@inbo.be>"

## for apt to be noninteractive
ENV DEBIAN_FRONTEND noninteractive
ENV DEBCONF_NONINTERACTIVE_SEEN true

## Set a default user. Available via runtime flag `--user docker`
## Add user to 'staff' group, granting them write privileges to /usr/local/lib/R/site.library
## User should also have & own a home directory (for rstudio or linked volumes to work properly).
RUN useradd docker \
  && mkdir /home/docker \
  && chown docker:docker /home/docker \
  && addgroup docker staff

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN  apt-get update \
  && apt-get install -y  --no-install-recommends \
     locales \
  && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
  && locale-gen nl_BE.utf8 \
  && /usr/sbin/update-locale LANG=nl_BE.UTF-8

ENV LC_ALL nl_BE.UTF-8
ENV LANG nl_BE.UTF-8

## Add apt-get repositories for R
RUN  apt-get update \
  && apt-get install -y --no-install-recommends \
      gnupg \
      ca-certificates \
  && sh -c 'echo "deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/" >> /etc/apt/sources.list' \
  && gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
  && gpg -a --export E298A3A825C0D65DFD57CBB651716619E084DAB9 | apt-key add -

## Install R base
RUN  apt-get update \
  && apt-get install -y --no-install-recommends \
    r-base-core=4.1.0-1.2004.0 \
    r-base-dev=4.1.0-1.2004.0 \
    r-cran-boot=1.3-28-1cran1.2004.0 \
    r-cran-class=7.3-19-1.2004.0 \
    r-cran-cluster=2.1.2-1.2004.0 \
    r-cran-codetools=0.2-18-1cran1.2004.0 \
    r-cran-foreign=0.8.81-1.2004.0 \
    r-cran-kernsmooth=2.23-20-1cran1.2004.0 \
    r-cran-lattice=0.20-44-1cran2~ubuntu20.04.1~ppa1 \
    r-cran-mass=7.3-54-1.2004.0 \
    r-cran-matrix=1.3-3-1.2004.0 \
    r-cran-mgcv=1.8-35-1cran1.2004.0 \
    r-cran-nlme=3.1.152-1.2004.0 \
    r-cran-nnet=7.3-16-1.2004.0 \
    r-cran-rpart=4.1-15-2focal0 \
    r-cran-spatial=7.3-11-2focal0 \
    r-cran-survival=3.2-11-1cran1.2004.0 \
    r-recommended=4.1.0-1.2004.0

## Install wget
RUN  apt-get update \
  && apt-get install -y --no-install-recommends \
    wget

## Install pandoc
RUN  wget https://github.com/jgm/pandoc/releases/download/2.7.3/pandoc-2.7.3-1-amd64.deb \
  && dpkg -i pandoc-2.7.3-1-amd64.deb \
  && rm pandoc-2.7.3-1-amd64.deb

## Install git
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    git \
    openssh-client

## Install curl dependencies
RUN  apt-get update \
  && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev

## Install ggplot2 dependencies
RUN  apt-get update \
  && apt-get install -y --no-install-recommends \
    libfreetype6-dev

## Install rgdal dependencies
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    gdal-bin \
    libgdal-dev \
    libproj-dev \
    proj-bin

## Install openssl dependencies
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libssl-dev

## Install systemfonts dependencies
RUN  apt-get update \
  && apt-get install -y --no-install-recommends \
    libfontconfig1-dev

## Install utils dependencies
RUN  apt-get update \
  && apt-get install -y --no-install-recommends \
    libudunits2-dev

## Install V8 dependencies
RUN  apt-get update \
  && apt-get install -y --no-install-recommends \
     libv8-dev

## Install xml2 dependencies
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libxml2-dev

WORKDIR /github/workspace

RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "renv::consent(provided = TRUE)"
COPY renv.lock renv.lock
RUN R -e "renv::restore()"
RUN R -e "renv::isolate()"

COPY entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
