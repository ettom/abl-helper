# This file is meant to be run from the root directory of the project using
# DOCKER_BUILDKIT=1 docker build -o out .
# The resulting statically linked binary will reside in out/
ARG PROJECT_NAME=abl-helper

FROM ettom/haskell-builder-base:latest AS build-stage
ARG PROJECT_NAME

# Install deps
RUN apk add ncurses-static

# Copy the code
COPY . /root/$PROJECT_NAME
WORKDIR /root/$PROJECT_NAME

# Hack to set ld-options
RUN sed -i 's|Haskell2010|Haskell2010\n  ld-options: -static|g' ./$PROJECT_NAME.cabal

# Build & install
RUN stack install \
	--skip-ghc-check --local-bin-path /root/$PROJECT_NAME/target \
	--split-objs --ghc-options='-fPIC'


FROM scratch AS export-stage
ARG PROJECT_NAME
COPY --from=build-stage /root/$PROJECT_NAME/target /
