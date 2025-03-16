#!/usr/bin/env bash

# file_tar_gz=$1 # ex: ideaIU-*.tar.gz
# unarchived_dir=$2 # ex: ideaIU-*
# app_name=$3 # ex: intellij

file_tar_gz="JetBrains.Rider-*.tar.gz"
unarchived_dir="JetBrains.Rider-*"
app_name="rider"

#cd ~/Downloads && \
    #tar -xzf "$file_tar_gz" && \
    rm "$file_tar_gz" ; \
    sudo mv "$unarchived_dir" "/opt/$app_name"  && \
    sudo ln -s "/opt/$app_name/bin/$app_name" "/usr/local/bin/$app_name"

# update
sudo rm -r "/opt/$app_name"
cd ~/Downloads && \
    tar -xzf "$file_tar_gz" && \
    sudo mv "$unarchived_dir" "/opt/$app_name"

# if installed
#sudo mv /opt/intellij /opt/old-intellij && \
    #sudo rm /usr/share/applications/intellij-idea.desktop && \
    #sudo mv /usr/local/bin/intellij-idea-ultimate /usr/local/bin/intellij-idea-ultimate1

desktop_entry="[Desktop Entry]
Version=1.0
Type=Application
Name=$app_name
Icon=/opt/$app_name/bin/$app_name.png
Exec=\"/usr/local/bin/$app_name\" %f
Comment=Integrated Development Environment
Categories=Development;IDE;
Terminal=false
StartupNotify=true
"

desktop_app_file="/usr/share/applications/$app_name.desktop"
echo "$desktop_entry" | sudo tee "$desktop_app_file" > /dev/null
sudo update-desktop-database
echo "Desktop entry created at: $desktop_app_file"




# desktop_entry="[Desktop Entry]
# Version=1.0
# Type=Application
# Name=rider
# Icon=/opt/rider/bin/rider.png
# Exec=\"/usr/local/bin/$app_name\" %f
# Comment=Integrated Development Environment
# Categories=Development;IDE;
# Terminal=false
# StartupNotify=true
# "
#
# desktop_app_file="/usr/share/applications/rider.desktop"
# echo "$desktop_entry" | sudo tee "$desktop_app_file" > /dev/null
# sudo update-desktop-database
# echo "Desktop entry created at: $desktop_app_file"