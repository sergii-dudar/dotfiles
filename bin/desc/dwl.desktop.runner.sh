slstatus -s | exec dwl -s "\
    /home/serhii/dotfiles/bin/apply-display-settings.w.sh dwl; \
    /home/serhii/dotfiles/bin/wmscripts/autostart_once.w.sh dwl; \
    /home/serhii/dotfiles/bin/wmscripts/autostart_always.w.sh dwl"