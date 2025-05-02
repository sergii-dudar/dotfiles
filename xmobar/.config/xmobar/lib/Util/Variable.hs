module Util.Variable where

import GHC.IO.Unsafe (unsafePerformIO)
import System.Directory
import System.Environment (getEnv)

-- ################## CONSTANTS #############################

{-# NOINLINE homeDir #-}
homeDir = unsafePerformIO getHomeDirectory ++ "/"
dotfilesDir = homeDir ++ "dotfiles/"
xmobarHomeDir = dotfilesDir ++ "xmobar/.config/xmobar/"
xmobarShellDir = xmobarHomeDir ++ "shell/"
xmobarResourcesDir = xmobarHomeDir ++ "resources"
rofiDir = dotfilesDir ++ "rofi/.config/rofi/scripts/"

-- ################## APPS #############################

appsGhostty = "sh -c 'cd ~ && exec ghostty'"
appsKitty = "sh -c 'cd ~ && exec kitty'"
appsVolumeControl = "pavucontrol"
appsBookReader = "evince"
appsIntellij = "intellij-idea-ultimate"
appsQbittorrent = "env QT_SCALE_FACTOR=1.4 qbittorrent"
appsBrowser = "brave"
appsMonkeyType = "brave --profile-directory=Default --app-id=picebhhlijnlefeleilfbanaghjlkkna"

appsGnomeSystemMonitor = "gnome-system-monitor"
appsGnomeSettings = "XDG_CURRENT_DESKTOP=GNOME gnome-control-center"
appsGnomeClocks = "gnome-clocks"
appsGnomeCalendar = "gnome-calendar"

appsTerminalHtop = "kitty --name htop_info -e htop"
appsTerminalDiscUsage = "kitty --name disc_usage_info --hold zsh -c \"export MANUAL_RL=1; df; exec zsh\""
appsTerminalDiscGdu = "kitty --name disc_ugd -e gdu ~"

runChangeLanguage = "~/dotfiles/bin/change_language.sh"
runVolumeToggle = xmobarShellDir ++ "volume-toggle.sh"
runVolumeIncrease = xmobarShellDir ++ "volume-increase.sh"
runVolumeDecrease = xmobarShellDir ++ "volume-decrease.sh"
runWeatherWeb = "brave https://sinoptik.ua/pohoda/vinnytsia"

menusApps = rofiDir ++ "launcher_t1"
menusPower = rofiDir ++ "powermenu_t1"

modulesOpenWeather = xmobarShellDir ++ "module.open-weather"

-- ################## FONTS #############################

toNerdFont :: Int -> String
toNerdFont size = "CaskaydiaCove Nerd Font Bold " ++ show size

toEmojiFont :: Int -> String
toEmojiFont size = "Noto Color Emoji " ++ show size

defaultFont :: String
defaultFont = toNerdFont 18

additionalFonts :: [String]
additionalFonts =
    [ toNerdFont 20 -- 1
    , toNerdFont 22 -- 2
    , toNerdFont 24 -- 3
    , toNerdFont 26 -- 4
    , toNerdFont 28 -- 5
    , toNerdFont 30 -- 6
    , toNerdFont 32 -- 7
    , toNerdFont 34 -- 8
    , toNerdFont 36 -- 9
    , toNerdFont 38 -- 10
    , toEmojiFont 20 -- 11
    , toEmojiFont 22 -- 12
    , toEmojiFont 24 -- 13
    , toEmojiFont 26 -- 14
    , toEmojiFont 28 -- 15
    , toEmojiFont 30 -- 16
    , toEmojiFont 32 -- 17
    , toEmojiFont 34 -- 18
    , toEmojiFont 36 -- 19
    , toEmojiFont 38 -- 20
    ]