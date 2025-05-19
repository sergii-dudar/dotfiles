# MPD & NCMPCC install

Hyprland config and related tools manages directly by `ln` command
instead `stow` to not mess in root config dir

```bash
# ============ ncmpcpp ============
sudo ln -s ~/dotfiles/mpd-config/ncmpcpp ~/.config/ncmpcpp

# ============== mpd ==============
# ==== LINUX (note that as config in user dir, service should be run as service user (--user))
sudo ln -s ~/dotfiles/mpd-config/mpd ~/.config/mpd
mkdir ~/.config/mpd/playlists

# link music folders to be in ~/Music
# sudo ln -s /mnt/ssd500/Music/ ~/Music/MusicLink

systemctl --user enable mpd
systemctl --user start mpd

# ==== MACOS (https://computingforgeeks.com/install-configure-mpd-ncmpcpp-macos/)
brew install mpd mpc ncmpcpp terminal-notifier
sudo ln -s ~/dotfiles/mpd-config/mpd-osx ~/.mpd
mkdir ~/.config/mpd/playlists

brew services start mpd
brew services stop mpd

```