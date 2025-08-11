# My dotfiles

All dotfile catalogs structured to be used as symlinks by [gnu stow](https://www.gnu.org/software/stow/), where config location is - `[app name]/[path to config from home dir that should be symlinked from repo catalog]`, for example `nvim/.config/nvim`,
where `stow nvim` will create symlink under `~/.config/nvim` with link to
`~/dotfiles/nvim/.config/nvim`. Tools that not managed by stow, located in [nonhome](nonhome)

All scripts that using by dot configurations required to have `dotfiles` to be cloned directly in `$HOME` directory as `~/dotfiles`

Some of my configurations (terminals etc) are using wallpapers that can be found here [my wallpaper collection](https://gitlab.com/Serhii.Dudar1/wallpapers).
It's also required to have wallpaper in home directory `~/wallpapers`

## My current tool preferences:
- Terminal: [ghostty](ghostty), [kitty](kitty), [wezterm](wezterm)
- Multiplexer: [tmux](tmux) + [tmux-powerline](tmux-powerline), [sesh](https://github.com/joshmedeski/sesh) (as sessions manager + own customizations based on it and tmux api)
- SHELL: [zsh](zsh) + [ohmyz](https://ohmyz.sh/) + [starship](https://starship.rs/) + [fastfetch](https://github.com/fastfetch-cli/fastfetch) and so on
- Code: [nvim](nvim), [intellij](idea) (only for working tasks, driven by [.ideavimrc](idea/.ideavimrc))
- Nerd-Fonts: [CascadiaCode](https://www.programmingfonts.org/#cascadia-code), [hack](https://www.programmingfonts.org/#hack), [FiraCode](https://www.programmingfonts.org/#firacode), [JetBrainsMono](https://www.programmingfonts.org/#jetbrainsmono)
- File Manager: [yazi](yazi), (used [ranger](ranger) in past)
- OS: linux (arch, ubuntu), macos
- Tiling WM: linux (Wayland) - [hyprland](hyprland) + [waybar-hyprland](waybar/.config/waybar/hyprland-config.jsonc), [sway](sway) + [waybar-sway](waybar/.config/waybar/sway-config.jsonc) 
- Tiling WM: linux (X11) - [xmonad](xmonad) + [xmobar](xmobar), [awesome](awesome), [qtile](qtile), [bspwm](bspwm) + [polybar](polybar), [i3](i3) + [polybar](polybar), [DWM](https://github.com/sergii-dudar/my-dwm) + [dwmblocks-async](suckless/dwmblocks-async)
- X11 compositor: [picom](picom)
- Tiling WM: macos - [aerospace](aerospace) + [sketchybar](sketchybar) + [janky-borders](janky-borders)
- Neovim: [LazyVim](https://www.lazyvim.org/) based [configuration](nvim/.config/nvim)
- Keyboard OS level tools: macos - [karabiner](karabiner), linux - [keyd](nonhome/keyd) / [kmonad](nonhome/kmonad)
- HotKeys launchers daemon: macos - using [aerospace](aerospace) support, X11 - [sxhkd](sxhkd)
- Launchers (Wayland, X11): [rofi](rofi) by using [fork with wayland support](https://github.com/in0ni/rofi-wayland)
- Notifications: X11 - [dunst](dunst), Wayland - [swaync](swaync), macos - builtin & [terminal-notifier](https://github.com/julienXX/terminal-notifier)
- Screen color temperature: X11 - [redshift](redshift), sway -[gammastep](gammastep), hyprland - [hyprsunset](hyprland/hypr/scripts/hyprsunset.runner)  
- Cron tasks: [cronie](cron) 
- Music & Players: [rmpc](mpd-config/rmpc), [mpd-linux](mpd-config/mpd), [mpd-macos](mpd-config/mpd-osx), [mpv](mpv), [ncmpcpp](mpd-config/ncmpcpp), [cmus](cmus)
- Other tools: [btop](btop), [fastfetch](fastfetch), [k9s](k9s), [lazygit](lazygit), [zellij](zellij)

## Configs readme with screenshots:

### Tiling Window Managers:
- [Hyprland](hyprland/README.md)
- [Aerospace](aerospace/.config/aerospace/README.md) 
- [Xmonad](xmonad/.config/xmonad/README.md)
- [DWM](suckless/DWM_README.md)
- [Sway](sway/.config/sway/README.md)
- [Awesome](awesome/.config/awesome/README.md) 
- [Bspwm](bspwm/.config/bspwm/README.md)
- [Qtile](qtile/.config/qtile/README.md)
- [i3wm](i3/.config/i3/README.md)

### Other:
- [Nvim](nvim/.config/README.md)
- [Tmux](tmux/README.md)
- [Fastfetch](fastfetch/.config/fastfetch/README.md)
- [MPD Ncmpcpp](mpd-config/ncmpcpp/README.md)
- [MPD Rmpc](mpd-config/rmpc/README.md)
- [Rofi](rofi/.config/rofi/README.md)
- [Yazi](yazi/.config/yazi/README.md)
- [Ranger](ranger/.config/ranger/README.md)
