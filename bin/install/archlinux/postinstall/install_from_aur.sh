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
    #yay -S telegram-desktop# from pacman
#yay -S viber# appimage from off site
yay -S appimagelauncher && \

    # https://github.com/jstkdng/ueberzugpp
yay -S ueberzugpp && \
    yay -S sesh-bin && \
    yay -S i3lock-color
yay -S networkmanager-dmenu-git
yay -S i3-back

# -------------------------------------------------------------------
# GTK drakula theme to gtk4 (pacman `gtk-section` must be installed!)
# -------------------------------------------------------------------
yay -S dracula-gtk-theme
yay -S dracula-icons-theme
# in `GTK Settings` apply dracula `theme` and `icons`
# run:
rm ~/.config/gtk-4.0/gtk.css
cp /usr/share/themes/Dracula/gtk-4.0/gtk.css ~/.config/gtk-4.0/gtk.css
cp /usr/share/themes/Dracula/gtk-4.0/gtk-dark.css ~/.config/gtk-4.0/gtk-dark.css