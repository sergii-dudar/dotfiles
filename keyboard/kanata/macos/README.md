# Configs

```bash
# 1. Install Karabiner-Elements (https://karabiner-elements.pqrs.org/) (it will intall Karabiner-DriverKit-VirtualHIDDevice required to kanata)
# run karabiner-elements and add all needed permissions (most important: `System Settings > Privacy & Security > Input Monitoring`)
# after it done, close karabiner, and make it not autostar (itt not needed anymore). it need, as we cant install raw karabiner driverkin, as it's not trusted dev source, and on corporative machines, with configured `Allow applications from: App store & Known Developers`, and impossible to install.

# 2. Install katana
cargo install --features cmd --locked kanata
ln -s ~/dotfiles/keyboard/kanata/macos ~/.config/kanata

# firstly run from terminal with sudo (check config, and asks required permissions (apply all that asking))
sudo kanata --cfg ~/.config/kanata/kanata.kbd

# important to add kanata cargo binary to:
# `System Settings > Privacy & Security > Input Monitoring`
# and possible to: `System Settings → Privacy & Security → Accessibility → Add Kanata`
# NOTE: some time security flags applying not from first time correctly that cause to have issue with access to input driver by LaunchDaemon, can be fixed with on\off permissions flags again.

# 3. In case 2 step successful, install kanata as LaunchDaemon with root permissions (to rut as start)
./setup-kanata-system.sh

# Additional
# - in case change something in `com.serhii.kanata.plist`, run ./restart-service.sh

```

Useful links (discussions):

- <https://github.com/jtroo/kanata/discussions/1537>
- <https://github.com/jtroo/kanata/issues/1264#issuecomment-2763085239>
