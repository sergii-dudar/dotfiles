-- IMPORTS

-- base
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks (avoidStruts, docks, docksEventHook, manageDocks)
import XMonad.StackSet qualified as W
import XMonad.Util.NamedScratchpad

-- actions

import Colors.GruvboxDarkShapes
import XMonad.Actions.CycleWS (WSType (..), moveTo, nextScreen, prevScreen, shiftTo)
import XMonad.Actions.WithAll (killAll, sinkAll)

-- utils

import XMonad.Actions.Warp
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)

-- data
import Data.Map qualified as M
import Data.Monoid
import Data.Ratio ((%)) -- for video
-- hooks

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, doRectFloat, isDialog, isFullscreen)
import XMonad.Util.SpawnOnce

-- layout

import XMonad.Hooks.InsertPosition (Focus (Newer), Position (End), insertPosition)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.GridVariants
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing

-- system
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)

-- myWorkspaces = clickable . (map xmobarEscape) $ [" 1 "," 2 "," 3 "," 4 "," 5 "," 6 "," 7 "," 8 "," 9 "]
myWorkspaces = clickable . (map xmobarEscape) $ [" \984819 ", " \62601 ", " \58930 ", " \987057 ", " \983502 ", " \984423 ", " \59007 ", " \58878 ", " \984687 "]
  where
    -- myWorkspaces = clickable . (map xmobarEscape) $ [" \61820 "," \62057 "," \61728 "," \61485 "," \61729 "," \61734 "," \62081 "," \61799 "," \61459 "]

    clickable l =
        [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>"
        | (i, ws) <- zip [1 .. 9] l
        , let n = i
        ]

-- main
main = do
    xmonad $
        ewmh $
            docks $
                def
                    { manageHook = (isFullscreen --> doFullFloat) <+> (isDialog --> doF W.swapUp) <+> manageDocks
                    , logHook =
                        dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $
                            xmobarPP
                                { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x >> hPutStrLn xmproc2 x
                                , ppCurrent = xmobarColor yellow "" . wrap "" ""
                                , ppVisible = xmobarColor green ""
                                , ppHidden = xmobarColor magenta "" . wrap "" ""
                                , --                        , ppHiddenNoWindows = xmobarColor  myppHiddenNoWindows ""
                                  ppTitle = xmobarColor yellow "" . shorten 45
                                , ppSep = " - "
                                , ppUrgent = xmobarColor red "" . wrap "!" "!"
                                , ppOrder = \(ws : _ : _) -> [ws]
                                }
                    }