#!/usr/bin/env bash

sudo launchctl bootstrap system /Library/LaunchDaemons/com.serhii.kanata.plist ; \
    sudo launchctl enable system/com.serhii.kanata.plist ; \
    sudo launchctl start com.serhii.kanata ; \
    tail -f /var/log/kanata.log