# Configs

```bash
# ========== install
cargo install --features cmd --locked kanata

# ========== put config
sudo ln -s ~/dotfiles/keyboard/kanata/linux ~/.config/kanata

# run
kanata --cfg ~/.config/kanata/kanata.kbd
# to run without sudo run ./no-sudo.sh

# NOTE: in case kayd enabled, need disable to avoid conflicts
sudo systemctl stop keyd
sudo systemctl disable keyd

### install as systemd user service (non root)
./setup-kanata.sh

# logs from service can be checked by
tail -f /home/serhii/.local/share/kanata/kanata.log

```

## Troubleshooting

There possible cases where ./no-sudo.sh can be not applied correctly, and after relogin\restart
kanata still can't be runned without `sudo`.

Possible reasons:

1. run `udevadm test /dev/uinput` and we should NOT see:
   `/etc/udev/rules.d/99-uinput.rules:1 Group 'uinput' is not a system group, ignoring.` (That happens when the uinput group exists but has a GID ≥ 1000. System users & groups: IDs from 1–999)

we should see:
`uinput: /etc/udev/rules.d/99-uinput.rules:1 GROUP="uinput": Set group ID: 959
uinput: /etc/udev/rules.d/99-uinput.rules:1 MODE="0660": Set mode: 0660`

---

FIX:

We need delete system group, and recreate:

`sudo groupdel uinput
sudo groupadd -r uinput
sudo usermod -aG uinput serhii`

Then log out/in (important).

after relogin\restart

`sudo udevadm control --reload-rules
sudo udevadm trigger /dev/uinput`

and check again
`getent group uinput (shoule be something like: uinput:x:959:serhii - id < 1000!)
udevadm test /dev/uinput
udevadm test /dev/uinput 2>&1 | grep GROUP
`
