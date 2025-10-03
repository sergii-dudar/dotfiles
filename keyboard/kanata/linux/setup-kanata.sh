#!/usr/bin/env bash
set -euo pipefail

SERVICE_DIR="$HOME/.config/systemd/user"
SERVICE_FILE="$SERVICE_DIR/kanata.service"
KANATA_BIN="$HOME/.cargo/bin/kanata"
KANATA_CFG="$HOME/.config/kanata/kanata.kbd"
LOG_DIR="$HOME/.local/share/kanata"
LOG_FILE="$LOG_DIR/kanata.log"

# Ensure directories exist
mkdir -p "$SERVICE_DIR"
mkdir -p "$LOG_DIR"

# Create systemd service if missing
if [[ ! -f "$SERVICE_FILE" ]]; then
    cat > "$SERVICE_FILE" <<EOF
[Unit]
Description=Kanata key remapper
After=graphical.target

[Service]
ExecStart=${KANATA_BIN} --cfg ${KANATA_CFG}
Restart=always
RestartSec=3
Requires=dev-uinput.device
After=dev-uinput.device

# Logging
StandardOutput=append:%h/.local/share/kanata/kanata.log
StandardError=append:%h/.local/share/kanata/kanata.log

[Install]
WantedBy=default.target
EOF
    echo "Created $SERVICE_FILE"
else
    echo "$SERVICE_FILE already exists, skipping creation."
    exit 0
fi

# Reload, enable, and start
systemctl --user daemon-reload
systemctl --user enable --now kanata.service

echo "Kanata service installed and started."
echo "Logs: $LOG_FILE"