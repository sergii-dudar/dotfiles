module Module.Variable where

import XMonad hiding (font, terminal)

-- ################## KEYS #############################

keysMod = mod4Mask
keysAlt = mod1Mask

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

appsTrayerRun = "~/dotfiles/xmobar/.config/xmobar/trayer/trayer-run"
appsTrayerToggle = "~/dotfiles/xmobar/.config/xmobar/trayer/trayer-toggle"

appsXmobarRun = "~/dotfiles/xmobar/.config/xmobar/xmobar-run"
appsXmobarToggle = "~/dotfiles/xmobar/.config/xmobar/xmobar-toggle"

-- ################## FONTS #############################

-- "CaskaydiaCove Nerd Font:bold:pixelsize=24",
toFont :: Int -> String
toFont size = "xft:CaskaydiaCove Nerd Font:style=Bold:size=" ++ show size

fontsDefault :: String
fontsDefault = toFont 16

-- ################## CONSTANTS #############################

settingsFloatFactorWidth :: Rational
settingsFloatFactorWidth = 0.75
settingsFloatFactorHeight :: Rational
settingsFloatFactorHeight = 0.8

-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
workspacesList = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]