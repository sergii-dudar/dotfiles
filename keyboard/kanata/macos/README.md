# Configs

```bash
# macos:
brew install kanata
ln -s ~/dotfiles/keyboard/kanata/macos ~/.config/kanata
sudo kanata --cfg ~/.config/kanata/kanata.kbd



```

NO sudo:

1. Run once manually from your user:
   `kanata --cfg ~/.config/kanata/kanata.kbd`

macOS will likely prompt: “Kanata” would like to control this computer using accessibility features.

2. Go to
   System Settings → Privacy & Security → Accessibility → Add Kanata
   and enable the toggle.

Once that’s granted, Kanata can run without sudo.

---

TEMP:

Notes

- This uses launchd (LaunchAgents) — the correct user-session way on macOS (no root, no sudo).

- It will automatically start on login.

- You can check its status anytime:

`launchctl list | grep kanata`

To remove it:

`launchctl unload ~/Library/LaunchAgents/com.serhii.kanata.plist
rm ~/Library/LaunchAgents/com.serhii.kanata.plist`
