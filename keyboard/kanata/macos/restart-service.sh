#!/usr/bin/env bash

sudo launchctl bootout system /Library/LaunchDaemons/com.serhii.kanata.plist ; \
    sudo launchctl bootstrap system /Library/LaunchDaemons/com.serhii.kanata.plist