# custom script that running in alacritty terminal, and used as desctop for startup app

desktop_entry="[Desktop Entry]
Version=1.0
Type=Application
Name=Run Alacritty Zellij
Icon=zellij
Exec=/usr/bin/alacritty -e zsh -i -c \"zellij\"
Comment=Manage Your Terminal Applications
Categories=ConsoleOnly;System;
Terminal=false
StartupNotify=true
X-GNOME-Autostart-Delay=2
"

desktop_file="/usr/share/applications/alacritty.zellij.desktop"
echo "$desktop_entry" | sudo tee "$desktop_file" > /dev/null
sudo update-desktop-database
echo "Desktop entry created at: $desktop_file"