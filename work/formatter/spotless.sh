#!/usr/bin/env bash

# exec java -jar "/home/serhii/.local/share/nvim/mason/packages/google-java-format/google-java-format-1.23.0-all-deps.jar" "$@"
exec mvn spotless:apply