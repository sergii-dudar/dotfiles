module Module.Xmobar (statusBarConfig, xmobarPPConfig) where

import XMonad

-- Hooks

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- Config Modules
import qualified Module.Variable as V
import qualified Util.Common as U

import XMonad.Util.EZConfig
import XMonad.Util.Loggers

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns

import XMonad.Hooks.EwmhDesktops
import XMonad.Util.ClickableWorkspaces (clickablePP)

statusBarConfig :: StatusBarConfig
statusBarConfig =
    statusBarProp V.appsXmobarRun
        . clickablePP
        . filterOutWsPP ["NSP"]
        $ xmobarPPConfig

xmobarPPConfig :: PP
xmobarPPConfig =
    def
        { ppSep = magenta " • "
        , ppTitleSanitize = xmobarStrip
        , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 4 . xmobarBorder "Bottom" "#8be9fd" 4
        , ppHidden = white . wrap " " ""
        , ppHiddenNoWindows = lowWhite . wrap " " ""
        , ppUrgent = red . wrap (yellow "!") (yellow "!")
        , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
        , ppExtras = [logTitles formatFocused formatUnfocused]
        }
    where
        formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
        formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

        -- \| Windows should have *some* title, which should not not exceed a
        -- sane length.
        ppWindow :: String -> String
        ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

        blue, lowWhite, magenta, red, white, yellow :: String -> String
        magenta = xmobarColor "#ff79c6" ""
        blue = xmobarColor "#bd93f9" ""
        white = xmobarColor "#f8f8f2" ""
        yellow = xmobarColor "#f1fa8c" ""
        red = xmobarColor "#ff5555" ""
        lowWhite = xmobarColor "#bbbbbb" ""