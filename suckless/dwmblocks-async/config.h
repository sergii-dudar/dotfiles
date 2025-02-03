#ifndef CONFIG_H
#define CONFIG_H

// String used to delimit block outputs in the status.
#define DELIMITER ""

// Maximum number of Unicode characters that a block can output.
#define MAX_BLOCK_OUTPUT_LENGTH 200 

// Control whether blocks are clickable.
#define CLICKABLE_BLOCKS 1

// Control whether a leading delimiter should be prepended to the status.
#define LEADING_DELIMITER 0

// Control whether a trailing delimiter should be appended to the status.
#define TRAILING_DELIMITER 0

// Define blocks for the status feed as X(icon, cmd, interval, signal).
#define BLOCKS(X) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-key-layout", 0, 1) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-volume", 0, 2) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-battery", 300, 3) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-memory", 30, 4) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-cpu-load", 30, 5) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-cpu-temp", 120, 6) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-disk", 1200, 7) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-weather", 1200, 8) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-date", 60, 10) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-runner 'env XDG_CURRENT_DESKTOP=GNOME gnome-control-center' '' '#3071db'", 0, 11) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-runner '/home/serhii/.config/rofi/scripts/launcher_t1' '󰣇' '#bd93f9'", 0, 13) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-runner '/home/serhii/.config/rofi/scripts/powermenu_t1' '⏻' '#d35f5e'", 0, 14) \
    X("", "/home/serhii/dotfiles/bin/dwm/sb-runner 'env QT_SCALE_FACTOR=1.4 qbittorrent' '󱑤' '#8caaee'", 0, 15)
#endif  // CONFIG_H

// update from sh: kill -(34 + signal) $(pidof dwmblocks)
// to update sb-key-layout: kill -(34 + 1) $(pidof dwmblocks)