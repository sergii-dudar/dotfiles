```bash
sudo tee /etc/udev/rules.d/10-bluetooth.rules <<EOF
ACTION=="add", SUBSYSTEM=="bluetooth", RUN+="/usr/sbin/rfkill unblock bluetooth"
EOF
```

# double check if `bluetooth` is not disabling by tlp or other power saving

```text
#DEVICES_TO_DISABLE_ON_STARTUP="bluetooth"
```

# may be needed

```bash
sudo tee /etc/systemd/system/bluetooth-unblock.service <<EOF
[Unit]
Description=Unblock Bluetooth
After=bluetooth.service

[Service]
Type=oneshot
ExecStart=/usr/sbin/rfkill unblock bluetooth

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl daemon-reload
# sudo systemctl enable bluetooth-unblock.service
sudo systemctl enable --now bluetooth-unblock.service
```
