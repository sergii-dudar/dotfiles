# MPD & NCMPCC install

```bash
# ============ ncmpcpp/rmpc ============
sudo ln -s ~/dotfiles/mpd-config/ncmpcpp ~/.config/ncmpcpp
sudo ln -s ~/dotfiles/mpd-config/rmpc ~/.config/rmpc

# ============== mpd ==============
# ==== LINUX (note that as config in user dir, service should be run as service user (--user))
sudo pacman -S mpd mpc ncmpcpp mpd-mpris
sudo ln -s ~/dotfiles/mpd-config/mpd ~/.config/mpd
mkdir ~/.config/mpd/playlists

# link music folders to be in ~/Music
# sudo ln -s /mnt/ssd500/Music/ ~/Music/MusicLink

systemctl --user --now enable mpd
systemctl --user --now enable mpd-mpris

# or by separate commands
# systemctl --user enable [mpd|mpd-mpris]
# systemctl --user start [mpd|mpd-mpris]

# ===============================================================================
# ==== MACOS (https://computingforgeeks.com/install-configure-mpd-ncmpcpp-macos/)
brew install mpd mpc ncmpcpp terminal-notifier
sudo ln -s ~/dotfiles/mpd-config/mpd-osx ~/.mpd
mkdir ~/.config/mpd/playlists

brew services start mpd
brew services stop mpd

```