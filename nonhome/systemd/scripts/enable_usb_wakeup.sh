#!/bin/bash

# Enable wake-up on USB devices
for device in /sys/bus/usb/devices/*/power/wakeup; do
    echo enabled | tee "$device"
done