#!/usr/bin/env bash

set -e

SERVICE_ID="com.serhii.kanata"
PLIST_PATH="$HOME/Library/LaunchAgents/$SERVICE_ID.plist"
KANATA_BIN="$HOME/.cargo/bin/kanata"
KANATA_CFG="$HOME/.config/kanata/kanata.kbd"
LOG_DIR="$HOME/.local/share/kanata"
LOG_FILE="$LOG_DIR/kanata.log"

mkdir -p "$LOG_DIR" "$HOME/Library/LaunchAgents"

if [ ! -x "$KANATA_BIN" ]; then
    echo "❌ Kanata binary not found at $KANATA_BIN"
    exit 1
fi

cat > "$PLIST_PATH" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
"http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
  <dict>
    <key>Label</key>
    <string>$SERVICE_ID</string>

    <key>ProgramArguments</key>
    <array>
      <string>$KANATA_BIN</string>
      <string>--cfg</string>
      <string>$KANATA_CFG</string>
    </array>

    <key>RunAtLoad</key><true/>
    <key>KeepAlive</key><true/>

    <key>StandardOutPath</key>
    <string>$LOG_FILE</string>
    <key>StandardErrorPath</key>
    <string>$LOG_FILE</string>
  </dict>
</plist>
EOF

echo "✅ Created LaunchAgent plist at $PLIST_PATH"

launchctl unload "$PLIST_PATH" 2>/dev/null || true
launchctl load "$PLIST_PATH"
launchctl start "$SERVICE_ID"

echo "🚀 Kanata autostart configured and launched."
echo "🪵 Logs: $LOG_FILE"