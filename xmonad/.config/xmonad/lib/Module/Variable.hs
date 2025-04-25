module Module.Variable where

import XMonad hiding (font, terminal)

-- ################## KEYS #############################

keysMod = mod4Mask
keysAlt = mod1Mask

-- ################## APPS #############################

appsTerminal = "ghostty"

-- ################## FONTS #############################

-- "CaskaydiaCove Nerd Font:bold:pixelsize=24",
toFont :: Int -> String
toFont size = "xft:CaskaydiaCove Nerd Font:style=Bold:size=" ++ show size

fontsDefault :: String
fontsDefault = toFont 16