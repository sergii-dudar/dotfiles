module Module.Variable where

import XMonad hiding (font, terminal)
import XMonad.Layout.StackTile (StackTile)

data Keys = Keys
    { modKey :: KeyMask
    , altKey :: KeyMask
    }

newtype Run = Run
    { terminal :: String
    }

data FontConf = FontConf
    { font :: String
    , fontsize :: Integer
    }

keys :: Keys
keys =
    Keys
        { modKey = mod4Mask
        , altKey = mod1Mask
        }

run :: Run
run =
    Run
        { terminal = "ghostty"
        }

fontConf :: FontConf
fontConf =
    FontConf
        { font = "xft:CaskaydiaCove Nerd Font:size=16:style=Bold"
        , fontsize = 16
        }