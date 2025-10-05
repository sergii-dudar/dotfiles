#!/usr/bin/env bash
set -euo pipefail

# Configuration
USER="iuada144"
DEAMON_NAME="com.serhii.kanata"
KANATA_BIN="/Users/$USER/.cargo/bin/kanata"
KANATA_CFG="/Users/$USER/.config/kanata/kanata.kbd"
PLIST_PATH="/Library/LaunchDaemons/$DEAMON_NAME.plist"
LOG_PATH="/var/log/kanata.log"

# Check for root
if [[ $EUID -ne 0 ]]; then
    echo "❌ This script must be run as root (use: sudo $0)"
    exit 1
fi

# Verify binary and config exist
if [[ ! -x "$KANATA_BIN" ]]; then
    echo "❌ Kanata binary not found at: $KANATA_BIN"
    exit 1
fi
if [[ ! -f "$KANATA_CFG" ]]; then
    echo "❌ Kanata config not found at: $KANATA_CFG"
    exit 1
fi

# === STOP AND REMOVE OLD DAEMON ===
if sudo launchctl list | grep -q "$DEAMON_NAME"; then
    echo "Stopping existing Kanata daemon..."
    sudo launchctl bootout system "$PLIST_PATH" || true
fi

if [ -f "$PLIST_PATH" ]; then
    echo "Removing old plist..."
    sudo rm -f "$PLIST_PATH"
fi

sudo rm -f "$LOG_PATH"

# === CREATE NEW PLIST ===
echo "📝 Creating LaunchDaemon plist at $PLIST_PATH..."
cat > "$PLIST_PATH" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>$DEAMON_NAME</string>

  <key>ProgramArguments</key>
  <array>
    <string>$KANATA_BIN</string>
    <string>--cfg</string>
    <string>$KANATA_CFG</string>
  </array>

  <key>RunAtLoad</key>
  <true/>

  <key>KeepAlive</key>
  <true/>

  <key>StandardOutPath</key>
  <string>$LOG_PATH</string>

  <key>StandardErrorPath</key>
  <string>$LOG_PATH</string>

  <key>UserName</key>
  <string>root</string>
</dict>
</plist>
EOF

# Set permissions
echo "🔐 Setting correct ownership and permissions..."
sudo chown root:wheel "$PLIST_PATH"
sudo chmod 644 "$PLIST_PATH"

# Create log file if not exists
sudo touch "$LOG_PATH"
sudo chmod 644 "$LOG_PATH"

# Load the daemon
echo "🚀 Loading Kanata LaunchDaemon..."
sudo launchctl bootstrap system "$PLIST_PATH"
sudo launchctl enable system/$DEAMON_NAME.plist
sudo launchctl start "$DEAMON_NAME"

echo "✅ Kanata LaunchDaemon installed and started."
echo "📄 Logs: $LOG_PATH"
echo "📄 Attaching to logs: $LOG_PATH (we can freely kill it, all oppparations finished succesfully)"
tail -f "$LOG_PATH"