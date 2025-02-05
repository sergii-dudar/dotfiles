# update distro
sudo pacman -Syu

sudo pacman -S zsh
sudo pacman -S tldr fzf fd
sudo pacman -S ripgrep bat tree
sudo pacman -S eza procs fastfetch
sudo pacman -S stow kitty wezterm git lazygit git-delta
sudo pacman -S nvim ranger rofi starship
sudo pacman -S tmux zellij flameshot
sudo pacman -S man-db ueberzugpp
sudo pacman -S wget k9s vlc qbittorrent
sudo pacman -S kolourpaint pinta evince
sudo pacman -S fuse # to run app images
sudo pacman -S ffmpegthumbnailer ffmpeg
sudo pacman -S usbutils eog p7zip zip, gum # 7-zip
sudo pacman -S feh autotiling sxhkd polybar picom i3 polkit-gnome pavucontrol volumeicon \
    alsa-utils pulseaudio-alsa xss-lock dunst pamixer
# dmenu
# sudo pacman -S yazi ffmpegthumbnailer unarchiver jq poppler fd ripgrep fzf zoxide
sudo pacman -S yazi unarchiver jq poppler zoxide bc
sudo pacman -S python-setuptools python-dbus-next # need for nvim Conform plugin
sudo pacman -S gpick xautolock
sudo pacman -S cmatrix ncdu gdu ghostty
sudo pacman -S mpv
sudo pacman -S sxiv

# lxappearance (nwg-look for x11, but nwg-look works on x11 well also)
# gtk-section
sudo pacman -S nwg-look xdg-desktop-portal \
    gnome-settings-daemon gnome-themes-extra xsettings-client xdg-desktop-portal-gtk
# catppuccin-gtk-theme-mocha

#music
sudo pacman -S playerctl

# curl -sS https://starship.rs/install.sh | sh

# detect keys
sudo pacman -S xorg-xev