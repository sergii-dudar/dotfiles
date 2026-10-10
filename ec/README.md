# ec (easy-conflict)

Theme file for [chojs23/ec](https://github.com/chojs23/ec): `.config/ec/themes.json`, read from `$XDG_CONFIG_HOME/ec/themes.json`
(`XDG_CONFIG_HOME` is `~/.config` in `zsh/serhii.shell/variables.sh`; without it macOS would use
`~/Library/Application Support/ec/`). Pure JSON, no comments allowed; an empty or invalid file makes ec fail at startup.

The built-in default theme is kept; only the titles are overridden (missing keys fall back to the defaults):
`title_fg` (pane titles, gruvbox yellow) and `selected_hunk_marker_fg` / `selected_hunk_marker_bg`, the one
fg/bg pair behind the `Conflict N [SELECTED|UNRESOLVED|RESOLVED]` labels (all three states share that background,
the state only changes the text colour). Key list and defaults: `ec` README, "Theme configuration".

```bash
cd ~/dotfiles && stow ec
```