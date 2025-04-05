module Module.Variable where

import XMonad hiding (terminal)

data Keys = Keys
    { modKey :: KeyMask
    , altKey :: KeyMask
    }

newtype Run = Run
    { terminal :: String
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