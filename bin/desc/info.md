`This desktop runners need only in cause using closed laptop
to be able to propectly disable laptop screen in closed LID
and properly start window manager after it`

`Need remove original *.desctop runner in /usr/share/xsessions with next commands`

`

sudo rm /usr/share/xsessions/qtile.desktop
sudo rm /usr/share/xsessions/i3.desktop

sudo cp ~/dotfiles/bin/desc/qtile.desktop /usr/share/xsessions/qtile.desktop
sudo cp ~/dotfiles/bin/desc/i3.desktop /usr/share/xsessions/i3.desktop

`

