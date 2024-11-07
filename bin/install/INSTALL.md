## Intellij
 - download gz file from https://www.jetbrains.com/idea/download/?section=linux
 - tar -xzf [intellij].tar.gz
 - sudo mv [intellij] /opt/intellij
 - sudo ln -s /opt/intellij/bin/idea.sh /usr/local/bin/intellij-idea-ultimate
 - sudo nvim /usr/share/applications/intellij-idea.desktop
```
[Desktop Entry]
Version=1.0
Type=Application
Name=IntelliJ IDEA
Icon=/opt/intellij/bin/idea.png
Exec="/opt/intellij/bin/idea.sh" %f
Comment=Integrated Development Environment
Categories=Development;IDE;
Terminal=false
StartupNotify=true
```
 - sudo update-desktop-database

## VsCode
 - download gz file from https://code.visualstudio.com/docs/?dv=linux64
 - tar -xzf [vscode].tar.gz
 - sudo mv [vscode] /opt/vscode
 - sudo ln -s /opt/vscode/code /usr/local/bin/code
 - sudo nvim /usr/share/applications/vscode.desktop
```
[Desktop Entry]
Version=1.0
Type=Application
Name=Visual Studio Code
Icon=/opt/vscode/resources/app/resources/linux/code.png
Exec="/opt/vscode/code" %f
Comment=Code Editing. Redefined.
Categories=Development;IDE;
Terminal=false
StartupNotify=true
```
 - sudo update-desktop-database

