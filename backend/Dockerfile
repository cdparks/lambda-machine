# Builder image
FROM fpco/stack-build-small:lts-18.8 AS builder
LABEL maintainer="Chris Parks <christopher.daniel.parks@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    gcc \
    libpq-dev \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*

# Build dependencies
RUN mkdir -p /src
WORKDIR /src
COPY stack.yaml package.yaml /src/
RUN stack install --dependencies-only

# Build app
COPY src /src/src
COPY app /src/app
RUN stack install

# Actual image
FROM ubuntu:18.04
LABEL maintainer="Chris Parks <christopher.daniel.parks@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    gcc \
    libpq-dev \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*

# Copy build artifact
RUN mkdir -p /app
WORKDIR /app
COPY --from=builder /root/.local/bin/serve /app/serve
