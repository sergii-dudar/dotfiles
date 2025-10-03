# Configs

```bash
# install
cargo install --locked kanata

# with cmd support
cargo install --features cmd --locked kanata

# config rule to run without sudo
# https://github.com/jtroo/kanata/wiki/Avoid-using-sudo-on-Linux

sudo groupadd uinput
sudo usermod -aG input $USER
sudo usermod -aG uinput $USER

echo 'KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"' | \
  sudo tee /etc/udev/rules.d/99-uinput.rules

# Then reload udev rules:
sudo udevadm control --reload-rules
sudo udevadm trigger

sudo modprobe uinput
# relogin

# -----------------------------
# verify
getfacl /dev/uinput


ls -l /dev/uinput
groups $USER

# -----------------------------
# put config
sudo ln -s ~/dotfiles/keyboard/kanata/linux ~/.config/kanata

# run
kanata --cfg ~/.config/kanata/kanata.kbd

# NOTE: in case kayd enabled, need disable to avoid conflicts
sudo systemctl stop keyd
sudo systemctl disable keyd

```