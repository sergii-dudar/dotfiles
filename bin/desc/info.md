`This desktop runners need only in cause using closed laptop
to be able to propectly disable laptop screen when closed LID
and properly start window manager after it, to properly detect active screens on first start`

`Need remove original *.desctop runner in /usr/share/xsessions with next commands`

`
sudo rm /usr/share/xsessions/qtile.desktop && \
    sudo cp ~/dotfiles/bin/desc/qtile.desktop /usr/share/xsessions/qtile.desktop

sudo rm /usr/share/xsessions/i3.desktop && \
    sudo cp ~/dotfiles/bin/desc/i3.desktop /usr/share/xsessions/i3.desktop

sudo rm /usr/share/xsessions/awesome.desktop && \
    sudo cp ~/dotfiles/bin/desc/awesome.desktop /usr/share/xsessions/awesome.desktop

`
