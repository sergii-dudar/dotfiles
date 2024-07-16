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
yay -S telegram-desktop
yay -S viber





