module Util.Variable where

-- ################## APPS #############################

appsTerminal = "ghostty"
appsVolumeControl = "pavucontrol"

appsGnomeSystemMonitor = "gnome-system-monitor"
appsGnomeSettings = "XDG_CURRENT_DESKTOP=GNOME gnome-control-center"
appsGnomeClocks = "gnome-clocks"
appsGnomeCalendar = "gnome-calendar"

appsTerminalHtop = "kitty --name htop_info -e htop"
appsTerminalDiscUsage = "kitty --name disc_usage_info --hold zsh -c \"export MANUAL_RL=1; df; exec zsh\""
appsTerminalDiscGdu = "kitty --name disc_ugd -e gdu"

appsTrayerRun = "~/dotfiles/xmobar/.config/xmobar/trayer/trayer-run.sh"
appsTrayerToggle = "~/dotfiles/xmobar/.config/xmobar/trayer/trayer-toggle.sh"

appsXmobarRun = "~/dotfiles/xmobar/.config/xmobar/xmobar-run.sh"
appsXmobarToggle = "~/dotfiles/xmobar/.config/xmobar/xmobar-toggle.sh"

-- ################## FONTS #############################

-- "CaskaydiaCove Nerd Font:bold:pixelsize=24",
toFont :: Int -> String
toFont size = "xft:CaskaydiaCove Nerd Font:style=Bold:size=" ++ show size

fontsDefault :: String
fontsDefault = toFont 16

-- ################## CONSTANTS #############################

space :: Int -> String
space pixeds = "<hspace=" ++ show pixeds ++ "/>"