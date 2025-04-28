module Module.Startup (runStartup) where

import XMonad

runStartup :: X ()
runStartup = do
    spawn "~/dotfiles/xmobar/.config/xmobar/trayer/trayer-run"

-- setWMName "LG3D"