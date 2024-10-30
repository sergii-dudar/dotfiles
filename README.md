## Collection of all dotfiles I use

All dotfile catalogs structured to be used as symlinks by [gnu stow](https://www.gnu.org/software/stow/)

All scripts that using by dot configurations required to have `dotfiles` to be cloned directly in `$HOME` directory as `~/dotfiles`

Some of my configurations (terminals etc) are using wallpapers that can be found here [my wallpaper collection](https://gitlab.com/Serhii.Dudar1/wallpapers).
It's also required to have wallpaper in home directory `~/wallpapers`

My current tool preferences:
 - Terminal: [kitty](kitty) + [tmux](tmux) + [nvim](nvim)
 - SHELL: [zsh](zsh) + [ohmyz](https://ohmyz.sh/) + [starship](https://starship.rs/) + [fastfetch](https://github.com/fastfetch-cli/fastfetch) and so on
 - Editor: [nvim](nvim)
 - Tmux extensions: [tmux-powerline](tmux/.tmux/plugins/tmux-powerline) (customized), [sesh](https://github.com/joshmedeski/sesh) (as sessions manager + own customizations based on it and tmux api)   
 - Nerd-Fonts: [hack](https://www.programmingfonts.org/#hack), [JetBrainsMono](https://www.programmingfonts.org/#jetbrainsmono)
 - File Manager: [yazi](yazi) (used [ranger](ranger) in past)
 - OS: linux (arch, ubuntu), macos
 - Tiling WM: macos - [aerospace](https://github.com/nikitabobko/AeroSpace), linux - [awesome](https://awesomewm.org/) / [qtile](https://qtile.org/), planning to try [hyprland](https://github.com/hyprwm/Hyprland)
 - Display server: [X11](https://www.x.org/wiki/) (Don't see that [Wayland](https://wayland.freedesktop.org/) is well-supported by tools I prefer for now)
 - Neovim: [LazyVim](https://www.lazyvim.org/) based [configuration](nvim/.config/nvim) with focus on effective working with many programming languages I'm working (java, gradle, maven, yaml, bash, lua, python, js/ts etc). My nvim configuration still evolving and I have many commented parts, but finished on ~85-90%, and I'm quite effectively using it in my daily workflow as `s/l java engineear`. Yes I still have to use intellij to do many specific tasks that have grate support in intellij (working with RDBMD, generation, previvion openapi etc), by using [.ideavimrc](idea/.ideavimrc) but in 80% cases I'm using neovim for all, especially for codding.
 - Keyboard programming mapping: macos - [karabiner](karabiner), linux - [keyd](nonhome/keyd)
 - Theme: [catppuccin-mocha](https://github.com/catppuccin/nvim)
 - Intellij: [.ideavimrc](idea/.ideavimrc), customized [catppuccin-mocha](idea/Catppuccin_Mocha.icls) theme

Why `Kitty`?

- `Alacritty` - In the past I have used [alacritty](alacritty) and it's awesome terminal emulator, and it's very fast, but no good support of image rendering that some time quite important to me, also have issues with installing, especially on `macos` (my working OS).

- `WezTerm` - [wezterm](wezterm) great and fast terminal emulator, and I had tried to use it, but have found image support not so good, and present some issues with proper color rendering\matching, especially dark colors, maybe it's OS related, maybe it's already fixed, but I have found, that in `kitty` it's implemented better for my point of view.

- `Iterm2` - In comparison with another terminals, it's quite slow for me, especially when working with neovim, not using GPU rendering that sometimes it's quite noticeable. Available only on macos, but I want to have configuration that working the same on both `macos` and `linux`.

- `Ghosty` - I'm waiting to a public beta to try it out, by reviews and information I have, can replace my preferred `kitty` choice :)

- `Kitty` - [kitty](kitty) for me at the moment, it's the best terminal to use daily with all features I need. great performance, GPU rendering, great image protocol implementation (that `Ghosty` also using), great and accurate color rendering and so on. I have only one point to `kitty` it's monospace font only, and I don't very like related issue with it - smalled font icons, but it's not critical for me with comparison to another terminals I had used before.

### Random screenshots (MACOS, AEROSPACE, KITTY, TMUX):

#### NEOVIM
![img.png](images/nvim.png)

### TMUX POPUP
![img.png](images/tmux_popup.png)

### TMUX SESSION MANAGER
![img.png](images/tmux_session_manager.png)

### YAZI
![img.png](images/yazi.png)

### Intellij
![intellij.png](images/intellij.png)


