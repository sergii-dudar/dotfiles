# install yay
cd ~/ && \
mkdir "tools" && \
cd "tools" && \
git clone https://aur.archlinux.org/yay.git && \
cd yay/ && \
makepkg -si && \
yay --version

# install packages from aut repository
yay -S sublime-text
yay -S google-chrome
#yay -S telegram-desktop# from pacman
#yay -S viber# appimage from off site
yay -S appimagelauncher

# https://github.com/jstkdng/ueberzugpp
yay -S ueberzugpp




