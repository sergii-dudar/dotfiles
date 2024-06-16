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

#clone repo
cd dotfiles

#install for example allacritty
stow allacritty

```

### Fonts
source: https://github.com/ryanoasis/nerd-fonts/releases

My favorite fonts: Hack Nerd Font: Regular|Bold

On mac can be installed by brew, or manually downloaded and imported by standard `Font Book` application

On linux the easiest way is
```
wget -P ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/Hack.zip \
&& cd ~/.local/share/fonts \
&& unzip JetBrainsMono.zip \
&& rm JetBrainsMono.zip \
&& fc-cache -fv

```

### Dotfiles external extensions.
To easily install dotfile tools extensions as third party themes, extension etc
from third party repos using directory `dotfiles.extensions` near `dotfiles`.