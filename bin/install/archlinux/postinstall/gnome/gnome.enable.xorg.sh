#!/bin/bash

# Define the file and the line to uncomment
file="/etc/gdm/custom.conf"
# "#WaylandEnable=false" or # WaylandEnable=false

sudo sed -i "s/^#\s*\(WaylandEnable=false\)/\1/" "$file"