# Configs

```bash
# install
cargo install --locked kanata

# config rule to run without sudo
sudo groupadd uinput
sudo usermod -aG input $USER
sudo usermod -aG uinput $USER

cd /etc/udev/rules.d
sudo nvim 99-uinput.rules
# KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"

# Then reload udev rules:
sudo udevadm control --reload-rules
sudo udevadm trigger

# -----------------------------
# verify
ls -l /dev/uinput
groups $USER

sudo modprobe uinput

# -----------------------------
# put config
sudo ln -s ~/dotfiles/keyboard/kanata/linux ~/.config/kanata

# run
kanata --cfg ~/.config/kanata/kanata.kbd

# NOTE: in case kayd enabled, need disable to avoid conflicts
sudo systemctl stop keyd
sudo systemctl disable keyd

```
