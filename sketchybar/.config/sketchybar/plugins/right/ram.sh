#!/usr/bin/env bash

LABEL=$(memory_pressure | grep "System-wide memory free percentage:" | awk '{print 100-$5"%"}')
sketchybar --set "$NAME" label="$LABEL"