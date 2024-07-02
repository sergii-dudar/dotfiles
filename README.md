my dot files, with ability to install by `stow`

As I'm using my dot files for many platforms (linux, macos)
I'm trying to use shared\common configurations like fonts etc
that will be working easily for both platforms.

In most cases, I'm trying to avoid as much as possible platform specific things.

### Installing dotfiles by `GNU stow`
Stow is very simple but powerful toos to manage dotfiles.
https://www.gnu.org/software/stow/

Good manuals can be found in YouTuber `typecraft`
https://medium.com/quick-programming/managing-dotfiles-with-gnu-stow-9b04c155ebad

```
brew install stow

cd ~/
git clone https://github.com/sergii-dudar/dotfiles.git
cd dotfiles

# install for example allacritty
stow allacritty

# in case we have existing config, that we want to override
# we need apply kind of hack, as stow now supported it fully from box

stow --adopt allacritty

# after this, we need restore in git dotfiles alacritty configs
# as --adopt just swapping existing configs with repo, and them make link with replacing existing config file 

```

### Fonts
source: https://github.com/ryanoasis/nerd-fonts/releases

My favorite fonts: Hack Nerd Font: Regular|Bold

On mac can be installed by brew, or manually downloaded and imported by standard `Font Book` application

On linux the easiest way is
```
https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/JetBrainsMono.zip
https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/Hack.zip

wget -P ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/JetBrainsMono.zip \
&& cd ~/.local/share/fonts \
&& unzip JetBrainsMono.zip \
&& rm JetBrainsMono.zip \
&& fc-cache -fv

```

### Dotfiles external extensions.
To easily install dotfile tools extensions as third party themes, extension etc
from third party repos using directory `dotfiles.extensions` near `dotfiles`.

### Dependencies
#### Ranger
 - https://github.com/maximtrp/ranger-archives
 - https://github.com/alexanderjeurissen/ranger_devicons

#### Alacritty
- https://github.com/alacritty/alacritty-theme

#### ZSH
- https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/INSTALL.md

#### tmux
- https://github.com/tmux-plugins/tpm

#### keyd
- https://github.com/rvaiya/keyd

#### neovim
- https://github.com/junegunn/vim-plug

#### https://flameshot.org/
#### https://github.com/keshavbhatt/glate
#### https://wiki.gnome.org/Apps/Rhythmbox

#### Key binds
##### System
 - Show the overview - disabled

##### Window
 - Activate the window menu - disabled
 - Close window - Shift+Super+C
 - Resize Window - Super+W
 - Toggle Fullscreen mode - Super+F11

##### Custom Shortcuts
 - Flameshot:           [ flameshot gui ]  key:Print
 - chrome:              [ google-chrome ]  key:Super+B
 - file manager:        [ nautilus ]  key:Super+F
 - intellij:            [ intellij-idea-ultimate ]  key:Super+I
   - ranger:            [ alacritty -e zsh -i -c "ranger ." ]  key:Super+R
     - rofi run:        [ rofi -show open ]  key:Super+S
 - terminal alacritty:  [ alacritty ]  key:Super+Enter

