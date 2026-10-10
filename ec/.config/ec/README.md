# ec (easy-conflict)

Full gruvbox theme for [chojs23/ec](https://github.com/chojs23/ec), kept in `gruvbox.themes.json` for reference; ec reads
only `themes.json` (currently the title-only override, see `../../README.md`), so `cp gruvbox.themes.json themes.json`
switches to it. The live file is read from `$XDG_CONFIG_HOME/ec/themes.json`
(`XDG_CONFIG_HOME` is `~/.config` in `zsh/serhii.shell/variables.sh`; without it macOS would use
`~/Library/Application Support/ec/`). Pure JSON, no comments allowed, and an empty or invalid file makes ec fail at startup.

`gruvbox-dark-hard`: gruvbox dark palette on the "hard" background (`bg0_h #1d2021`, the terminal supplies the
background itself), diff blends from gruvbox-material (`#34381b` added, `#402120` removed, `#0e363e` blue,
`#4f422e` yellow). Hex colours need a TrueColor terminal. Key list and defaults: `ec` README, "Theme configuration".

```bash
cd ~/dotfiles && stow ec
```