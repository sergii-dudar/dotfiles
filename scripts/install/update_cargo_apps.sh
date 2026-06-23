#!/usr/bin/env bash

cargo install rmpc --locked && \
    cargo install --features cmd --locked kanata && \
    cargo install --force yazi-build