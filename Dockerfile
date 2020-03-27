FROM ubuntu:18.04
LABEL maintainer="josh.enders@gmail.com"

# non-interactive apt-get
ENV DEBIAN_FRONTEND=noninteractive \
    TZ=UTC

# apt-key, add-apt-repository depedencies
RUN apt-get update \
    && apt-get install \
        --assume-yes \
        --no-install-recommends \
            gnupg \
            software-properties-common

# https://cloud.r-project.org/bin/linux/ubuntu/README

# Install repo keys
RUN apt-key adv \
    --keyserver keyserver.ubuntu.com \
    --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

# Add repository sources
RUN add-apt-repository \
    'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'

# Install dependencies
RUN apt-get update \
    && apt-get install \
        --assume-yes \
        --no-install-recommends \
            r-base \
            r-base-dev \
    && rm -rf /var/lib/apt/lists/*

#ENTRYPOINT ["/entrypoint.py"]
