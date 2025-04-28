module Module.Startup (runStartup) where

import XMonad

runStartup :: X ()
runStartup = do
    spawn "~/dotfiles/xmobar/.config/xmobar/trayerrc"

-- setWMName "LG3D"