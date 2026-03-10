# My dotfiles

Just my personal dotfiles I'm using as for all my home Arch Linux machines (with various tiling window manager configs) as for my working machines as Software Engineer (mostly MacOS or Linux Debian based systems (mostly Ubuntu) )

---

My Arch Linux config random screen:
![arch linux random screen][0]

My MacOS config random screen:
![macos random screen][1]

---

Most of my dotfile catalogs structured to be used as symlinks by [gnu stow][2], where config location is - `[app name]/[path to config from home dir that should be symlinked from repo catalog]`, for example `nvim/.config/nvim`, where `stow nvim` will create symlink under `~/.config/nvim` with link to `~/dotfiles/nvim/.config/nvim`.

All scripts that are using by dot configurations required to have `dotfiles` to be cloned directly in `$HOME` directory as `~/dotfiles`

Some of my configurations (terminals etc) are using wallpapers that can be found here [my wallpaper collection][3]. It's also required to have wallpaper in home directory `~/wallpapers`

## My current tool preferences

- Terminal: [alacritty][4], [ghostty][5], [kitty][6], [wezterm][7]
- Multiplexer: [tmux][8] + [tmux-powerline][9], [sesh][10] (as sessions manager + own customizations based on it and tmux api)
- SHELL: [zsh][11] + [ohmyz][12] + [starship][13] + [fastfetch][14] and so on
- Code: [nvim][15], [intellij][16] (only for specific working tasks, mostly because of existing some useful plugins, where I have not found decent alternative in neovim, driven by [.ideavimrc][16]. But need to say that in free time, I'm trying to replicate some very important intellij functionality (for me and my work) in my neovim config (like [java-refactor][17], [junit][18], [blink-java-mapstruct][19]), and it's reason why my nvim config quite huge, especially [nvim/utils][20], maybe some day I will extract most of main implementations to separate plugins, will see)
- Nerd-Fonts: [CascadiaCode][21], [hack][22], [FiraCode][23], [JetBrainsMono][24]
- File Manager: [yazi][25], [ranger][26]
- OS: linux (arch, ubuntu), macos
- Tiling WM: linux (Wayland) - [hyprland][27] + [waybar-hyprland][28], [sway][29] + [waybar-sway][30]
- Tiling WM: linux (X11) - [xmonad][31] + [xmobar][32], [awesome][33], [qtile][34], [bspwm][35] + [polybar][36], [i3][37] + [polybar][36], [DWM][38] + [dwmblocks-async][39]
- X11 compositor: [picom][40]
- Tiling WM: macos - [aerospace][41] + [sketchybar][42] + [janky-borders][43]
- Neovim: [LazyVim][44] based [configuration][15]. It's my main editor as Software Engineer, and it's oriented to many languages I'm using professionally (java, c#, js, bash, python... but mostly as java backend dev right now), or as hobby (lua, c/c++, rust, haskell...).
- Neotim Theme - customized `gruvbox-material` or `everforest`, bd - `hard` (the most eyes comfortable themes I ever used for very long working sessions!), also I'm a big fan of `catppuccin-mocha` but my eyes not very like it to work 24/7 (too much blue for me) 😺
- Keyboard OS level tools: kanata [linux][45] and [macos][46] configs (in previous: macos - [karabiner][47], linux - [keyd][48] / [kmonad][49]
- HotKeys launchers daemon: macos - using [aerospace][41] support, X11 - [sxhkd][50]
- Launchers (Wayland, X11): [rofi][51] by using [fork with wayland support][52]
- Notifications: X11 - [dunst][53], Wayland - [swaync][54], macos - builtin & [terminal-notifier][55]
- Screen color temperature: X11 - [redshift][56], sway -[gammastep][57], hyprland - [hyprsunset][58]
- Cron tasks: [cronie][59]
- Music & Players: [rmpc][60], [mpd-linux][61], [mpd-macos][62], [mpv][63], [ncmpcpp][64], [cmus][65]
- Other tools: [btop][66], [fastfetch][14], [k9s][67], [lazygit][68], [zellij][69] - but in 99% of time I'm using tmux only

## Configs readme with config screenshots

### Tiling Window Managers

- [Hyprland][70]
- [Aerospace][71]
- [Xmonad][72]
- [DWM][73]
- [Sway][74]
- [Awesome][75]
- [Bspwm][76]
- [Qtile][77]
- [i3wm][78]

### Other

- [Nvim][79]
- [Tmux][80]
- [Fastfetch][81]
- [MPD Ncmpcpp][82]
- [MPD Rmpc][83]
- [Rofi][84]
- [Yazi][85]
- [Ranger][86]

## Links

[0]: screenshots/hyprland/hyprland-minterm-scrat.png
[1]: screenshots/aerospace/aerospace-miniterm-scratchpad.png
[2]: https://www.gnu.org/software/stow/
[3]: https://gitlab.com/Serhii.Dudar1/wallpapers
[4]: alacritty/.config/alacritty
[5]: ghostty/.config/ghostty
[6]: kitty/.config/kitty
[7]: wezterm/.config/wezterm
[8]: tmux
[9]: tmux-powerline/.config/tmux-powerline
[10]: https://github.com/joshmedeski/sesh
[11]: zsh
[12]: https://ohmyz.sh/
[13]: starship/.config/starship.toml
[14]: fastfetch/.config/fastfetch
[15]: nvim/.config/nvim
[16]: idea/.ideavimrc
[17]: nvim/.config/nvim/lua/modules/java/refactor
[18]: nvim/.config/nvim/lua/modules/java/junit
[19]: nvim/.config/nvim/lua/modules/java/mapstruct
[20]: nvim/.config/nvim/lua/utils
[21]: https://www.programmingfonts.org/#cascadia-code
[22]: https://www.programmingfonts.org/#hack
[23]: https://www.programmingfonts.org/#firacode
[24]: https://www.programmingfonts.org/#jetbrainsmono
[25]: yazi/.config/yazi
[26]: ranger/.config/ranger
[27]: hyprland
[28]: waybar/.config/waybar/hyprland-config.jsonc
[29]: sway/.config/sway
[30]: waybar/.config/waybar/sway-config.jsonc
[31]: xmonad/.config/xmonad
[32]: xmobar/.config/xmobar
[33]: awesome/.config/awesome
[34]: qtile/.config/qtile
[35]: bspwm/.config/bspwm
[36]: polybar/.config/polybar
[37]: i3/.config/i3
[38]: https://github.com/sergii-dudar/my-dwm
[39]: suckless/dwmblocks-async
[40]: picom/.config/picom
[41]: aerospace/.config/aerospace
[42]: sketchybar/.config/sketchybar
[43]: janky-borders/.config/borders
[44]: https://www.lazyvim.org/
[45]: keyboard/kanata/linux
[46]: /keyboard/kanata/macos
[47]: karabiner/.config/karabiner
[48]: nonhome/keyd
[49]: nonhome/kmonad
[50]: sxhkd/.config/sxhkd
[51]: rofi/.config/rofi
[52]: https://github.com/in0ni/rofi-wayland
[53]: dunst/.config/dunst
[54]: swaync/.config/swaync
[55]: https://github.com/julienXX/terminal-notifier
[56]: redshift/.config/redshift
[57]: gammastep/.config/gammastep
[58]: hyprland/hypr/scripts/hyprsunset.runner
[59]: cron
[60]: mpd-config/rmpc
[61]: mpd-config/mpd
[62]: mpd-config/mpd-osx
[63]: mpv/.config/mpv
[64]: mpd-config/ncmpcpp
[65]: cmus/.config/cmus
[66]: btop/.config/btop
[67]: k9s/.config/k9s
[68]: lazygit/.config/lazygit
[69]: zellij/.config/zellij
[70]: hyprland/README.md
[71]: aerospace/.config/aerospace/README.md
[72]: xmonad/.config/xmonad/README.md
[73]: suckless/DWM_README.md
[74]: sway/.config/sway/README.md
[75]: awesome/.config/awesome/README.md
[76]: bspwm/.config/bspwm/README.md
[77]: qtile/.config/qtile/README.md
[78]: i3/.config/i3/README.md
[79]: nvim/.config/nvim/README.md
[80]: tmux/README.md
[81]: fastfetch/.config/fastfetch/README.md
[82]: mpd-config/ncmpcpp/README.md
[83]: mpd-config/rmpc/README.md
[84]: rofi/.config/rofi/README.md
[85]: yazi/.config/yazi/README.md
[86]: ranger/.config/ranger/README.md
