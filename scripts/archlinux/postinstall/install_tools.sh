# pre install steps
# cd ~/Downloads
# download last version of tools as *.tar.gz:
# https://www.jetbrains.com/idea/download/?section=linux
# https://code.visualstudio.com/docs/?dv=linux64

# ==========================================================================
# ======================== intellij ==========================================
# ==========================================================================

cd ~/Downloads && \
tar -xzf ideaIU-*.tar.gz && \
rm ideaIU-*.tar.gz && \
sudo mv idea-IU* /opt/intellij && \
sudo ln -s /opt/intellij/bin/idea.sh /usr/local/bin/intellij-idea-ultimate

desktop_entry="[Desktop Entry]
Version=1.0
Type=Application
Name=IntelliJ IDEA
Icon=/opt/intellij/bin/idea.png
Exec=\"/opt/intellij/bin/idea.sh\" %f
Comment=Integrated Development Environment
Categories=Development;IDE;
Terminal=false
StartupNotify=true
"

desktop_intellij_file="/usr/share/applications/intellij-idea.desktop"
echo "$desktop_entry" | sudo tee "$desktop_intellij_file" > /dev/null
sudo update-desktop-database
echo "Desktop entry created at: $desktop_intellij_file"

# ==========================================================================
# ======================== vscode ==========================================
# ==========================================================================

cd ~/Downloads && \
tar -xzf code-*.tar.gz && \
rm code-*.tar.gz && \
sudo mv VSCode-* /opt/vscode && \
sudo ln -s /opt/vscode/code /usr/local/bin/code

desktop_vscode_entry="[Desktop Entry]
Version=1.0
Type=Application
Name=Visual Studio Code
Icon=/opt/vscode/resources/app/resources/linux/code.png
Exec=\"/opt/vscode/code\" %f
Comment=Code Editing. Redefined.
Categories=Development;IDE;
Terminal=false
StartupNotify=true
"

desktop_vs_code_file="/usr/share/applications/vscode.desktop"
echo "$desktop_vscode_entry" | sudo tee "$desktop_vs_code_file" > /dev/null
sudo update-desktop-database
echo "Desktop entry created at: $desktop_vs_code_file"


