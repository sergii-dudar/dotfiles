# terminal-themes

Interactive theme pickers, one per terminal, same behaviour everywhere: run the script of the terminal you are sitting
in, move through the list, every highlighted theme is applied to the window right away, `Enter` keeps it (written to
that terminal's config), `Esc` puts the previous one back. A preview pane shows the 16 colours and a text sample as the
terminal actually draws them. The scripts refuse to run in another terminal (inside tmux they identify the terminal
through tmux's `#{client_termtype}`, the only reliable source there) and need `fzf`.

| script | themes | live preview | Enter writes |
| --- | --- | --- | --- |
| `alacritty.sh` | `~/.config/alacritty/themes/themes/*.toml` (alacritty-theme repo) | rewrites the active theme line of `import = [...]`, alacritty reloads it itself | the same line |
| `ghostty.sh` | the app's resources + `~/.config/ghostty/themes` (what `ghostty +list-themes` shows) | OSC 4/10/11/12/17/19 escape codes, config overrides on top | `theme = NAME`; other windows after `reload_config` |
| `kitty.sh` | colour `*.conf` next to kitty.conf + the kitty-themes repo once `kitten themes` has cached it | rewrites the active `include ...conf` line and sends SIGUSR1 (kitty reloads) | the same line (repo themes go to `current-theme.conf`) |
| `wezterm.sh` | wezterm's built-in schemes (dumped once per version to `~/.cache/terminal-themes`) | rewrites the active `return "NAME"` of `get_color_scheme()` in appearance.lua, wezterm reloads it itself | the same line |
| `foot.sh` | `~/.config/foot/themes` + `/usr/share/foot/themes` | OSC escape codes, foot.ini overrides on top (unverified, no foot on macOS) | the `include=` line; new windows use it |

Config-rewrite previews show exactly the end result, including the colours each config sets on top of its theme
(background `#121212`, cursor, selection). The OSC previews (ghostty, foot) apply those overrides too, so they match.
Inside tmux the escape codes go through `allow-passthrough` (on in `.tmux.conf`).

Every script also works non-interactively: `list`, `current`, `apply NAME`, and for the OSC ones `preview NAME` /
`restore`. `common.sh` holds the shared parts (detection, fzf loop, OSC and rewrite helpers); `TT_TERMINAL`,
`TT_CONFIG` and `TT_TTY` override detection, config path and output tty for testing.
