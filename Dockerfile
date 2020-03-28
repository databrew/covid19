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
# http://keyserver.ubuntu.com/pks/lookup?op=get&search=0xe298a3a825c0d65dfd57cbb651716619e084dab9o

# Install repo keys
RUN apt-key adv \
    --keyserver keyserver.ubuntu.com \
    --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

# Add repository sources
RUN add-apt-repository \
    'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'

# Install R and R library dependencies
RUN apt-get update \
    && apt-get install \
        --assume-yes \
        --no-install-recommends \
            r-base \
            r-base-dev \
            curl \
            libcurl4-openssl-dev \
            libxml2-dev \
            libgdal-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy project files to /srv
COPY . /srv
WORKDIR /srv

# Install packrat
RUN R \
    -e 'install.packages(c("packrat"))'

# Install app dependencies via packrat
RUN R \
    -e 'packrat::restore()'

#ENTRYPOINT ["/entrypoint.py"]
