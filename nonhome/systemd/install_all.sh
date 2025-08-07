#!/usr/bin/env bash

# make link & load the new service definition & start the service immediately & enable the service to start at boot
#sudo ln -s /home/serhii/dotfiles/nonhome/systemd/enable_usb_wakeup.service /etc/systemd/system/enable_usb_wakeup.service && \

sudo cp /home/serhii/dotfiles/nonhome/systemd/enable_usb_wakeup.service /etc/systemd/system/enable_usb_wakeup.service && \
sudo systemctl daemon-reload && \
sudo systemctl enable enable_usb_wakeup.service && \
sudo systemctl start enable_usb_wakeup.service && \
sudo systemctl status enable_usb_wakeup.service

# check if usb ports to wake up enabled
# for device in /sys/bus/usb/devices/*/power/wakeup; do cat $device; done

# manual enabling
# for device in /sys/bus/usb/devices/*/power/wakeup; do echo enabled | sudo tee $device; done