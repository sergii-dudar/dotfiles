#!/usr/bin/env bash
set -e

# Ensure group exists
sudo groupadd -f uinput

# Add current user to uinput group
sudo usermod -aG uinput "$USER"

# Create proper udev rule
# cat <<EOF | sudo tee /etc/udev/rules.d/99-uinput.rules
# KERNEL=="uinput", MODE="0660", GROUP="uinput"
# EOF
echo 'KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"' | \
    sudo tee /etc/udev/rules.d/99-uinput.rules

# Reload udev rules
sudo udevadm control --reload-rules
sudo udevadm trigger

# Load kernel module (persistent after reboot if listed in /etc/modules)
sudo modprobe uinput

echo "✅ uinput group and rule installed."
echo "ℹ️  Log out and log back in (or reboot) for group membership to take effect."
