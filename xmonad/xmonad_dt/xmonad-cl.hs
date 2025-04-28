-- Base

import System.Directory
import System.Exit (exitSuccess)
import System.IO (hClose, hPutStr, hPutStrLn)
import XMonad
import XMonad.StackSet qualified as W

-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D (..), WSType (..), moveTo, nextScreen, prevScreen, shiftTo)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import XMonad.Actions.Search qualified as S
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (killAll, sinkAll)

-- Data
import Data.Char (isSpace, toUpper)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust)
import Data.Monoid
import Data.Tree

-- Hooks
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isFullscreen)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory

-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, single, (??))
import XMonad.Layout.MultiToggle qualified as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.ToggleLayouts qualified as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)
import XMonad.Layout.WindowNavigation

-- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Hacks (javaHack, trayAbovePanelEventHook, trayPaddingEventHook, trayPaddingXmobarEventHook, trayerAboveXmobarEventHook, trayerPaddingXmobarEventHook, windowedFullscreenFixEventHook)
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce


-- myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
myWorkspaces = [" dev ", " www ", " sys ", " doc ", " vbox ", " chat ", " mus ", " vid ", " gfx "]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1 ..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

main :: IO ()
main = do
    -- Launching three instances of xmobar on their monitors.
    xmproc0 <- spawnPipe ("xmobar -x 0 $HOME/.config/xmobar/" ++ colorScheme ++ "-xmobarrc")
    xmproc1 <- spawnPipe ("xmobar -x 1 $HOME/.config/xmobar/" ++ colorScheme ++ "-xmobarrc")
    xmproc2 <- spawnPipe ("xmobar -x 2 $HOME/.config/xmobar/" ++ colorScheme ++ "-xmobarrc")
    -- the xmonad, ya know...what the WM is named after!
    xmonad $
        addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) myKeys $
            ewmh $
                docks $
                    def
                        { 
                        , logHook =
                            dynamicLogWithPP $
                                filterOutWsPP [scratchpadWorkspaceTag] $
                                    xmobarPP
                                        { ppOutput = \x ->
                                            hPutStrLn xmproc0 x -- xmobar on monitor 1
                                                >> hPutStrLn xmproc1 x -- xmobar on monitor 2
                                                >> hPutStrLn xmproc2 x -- xmobar on monitor 3
                                        , ppCurrent =
                                            xmobarColor color06 ""
                                                . wrap
                                                    ("<box type=Bottom width=2 mb=2 color=" ++ color06 ++ ">")
                                                    "</box>"
                                        , -- Visible but not current workspace
                                          ppVisible = xmobarColor color06 "" . clickable
                                        , -- Hidden workspace
                                          ppHidden =
                                            xmobarColor color05 ""
                                                . wrap
                                                    ("<box type=Top width=2 mt=2 color=" ++ color05 ++ ">")
                                                    "</box>"
                                                . clickable
                                        , -- Hidden workspaces (no windows)
                                          ppHiddenNoWindows = xmobarColor color05 "" . clickable
                                        , -- Title of active window
                                          ppTitle = xmobarColor color16 "" . shorten 60
                                        , -- Separator character
                                          ppSep = "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>"
                                        , -- Urgent workspace
                                          ppUrgent = xmobarColor color02 "" . wrap "!" "!"
                                        , -- Adding # of windows on current workspace to the bar
                                          ppExtras = [windowCount]
                                        , -- order of things in xmobar
                                          ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
                                        }
                        }