# slstatus -s | exec dwl -s "\
dwlblocks | exec dwl -s "\
    /home/serhii/dotfiles/scripts/apply-display-settings.w.sh dwl; \
    /home/serhii/dotfiles/scripts/wmscripts/autostart_once.w.sh dwl; \
    /home/serhii/dotfiles/scripts/wmscripts/autostart_always.w.sh dwl"
