FROM ubuntu:18.04
LABEL maintainer="josh.enders@gmail.com"

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

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen en_US.utf8 \
    && /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

#ENTRYPOINT ["/entrypoint.py"]