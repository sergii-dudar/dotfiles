# Hyprland configs

Hyprland config and related tools manages directly by `ln` command
instead `stow` to not mess in root config dir

```bash
sudo ln -s ~/dotfiles/mpd-config/mpd ~/.config/mpd && \
sudo ln -s ~/dotfiles/mpd-config/ncmpcpp ~/.config/ncmpcpp

# MACOS (https://computingforgeeks.com/install-configure-mpd-ncmpcpp-macos/)
brew install mpd mpc ncmpcpp terminal-notifier
sudo ln -s ~/dotfiles/mpd-config/mpd-osx ~/.mpd

```
