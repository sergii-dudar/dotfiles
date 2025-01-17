# install yay
cd ~/ && \
    mkdir "tools" && \
    cd "tools" && \
    git clone https://aur.archlinux.org/yay.git && \
    cd yay/ && \
    makepkg -si && \
    yay --version && \

    # install packages from aut repository
yay -S sublime-text && \
    yay -S google-chrome && \
    yay -S brave-bin
#yay -S telegram-desktop# from pacman
#yay -S viber# appimage from off site
yay -S appimagelauncher && \

    # https://github.com/jstkdng/ueberzugpp
yay -S ueberzugpp && \
    yay -S sesh-bin && \
    yay -S i3lock-color
yay -S qtile-extras
yay -S networkmanager-dmenu-git
yay -S i3-back
# yay -S hollywood
yay -S insomnia
# -------------------------------------------------------------------
# GTK drakula theme to gtk4 (pacman `gtk-section` must be installed!)
# -------------------------------------------------------------------
yay -S dracula-gtk-theme dracula-icons-theme && \
    mv ~/.config/gtk-4.0 ~/.config/gtk-4.0.bak && \
    cp -r /usr/share/themes/Dracula/gtk-4.0 ~/.config/gtk-4.0
# after install, in `GTK Settings` apply dracula `theme` and `icons`

# haskell
yay -S ghcup-hs-bin # brew install ghcup
# ghcup install ghc 9.6.6
# ghcup set ghc 9.6.6
# (hls latest (2.9.0.1 at the moment))
#
# ghcup install stack 3.1.1
# ghcup set stack 3.1.1

yay -S clipboard # brew install clipboard