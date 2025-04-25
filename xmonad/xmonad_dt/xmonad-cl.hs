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

myScratchPads :: [NamedScratchpad]
myScratchPads =
    [ NS "terminal" spawnTerm findTerm manageTerm
    , NS "mocp" spawnMocp findMocp manageMocp
    , NS "calculator" spawnCalc findCalc manageCalc
    ]
  where
    spawnTerm = myTerminal ++ " -t scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnMocp = myTerminal ++ " -t mocp -e mocp"
    findMocp = title =? "mocp"
    manageMocp = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnCalc = "qalculate-gtk"
    findCalc = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.4
        t = 0.75 - h
        l = 0.70 - w

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme =
    def
        { swn_font = "xft:Ubuntu:bold:size=60"
        , swn_fade = 1.0
        , swn_bgcolor = "#1c1f24"
        , swn_color = "#ffffff"
        }

-- The layout hook
myLayoutHook =
    avoidStruts $
        mouseResize $
            windowArrange $
                T.toggleLayouts floats $
                    mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout =
        withBorder myBorderWidth tall
            ||| noBorders monocle
            ||| floats
            ||| noBorders tabs
            ||| grid
            ||| spirals
            ||| threeCol
            ||| threeRow
            ||| tallAccordion
            ||| wideAccordion

-- myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
myWorkspaces = [" dev ", " www ", " sys ", " doc ", " vbox ", " chat ", " mus ", " vid ", " gfx "]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1 ..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
    composeAll
        -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
        -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
        -- I'm doing it this way because otherwise I would have to write out the full
        -- name of my workspaces and the names would be very long if using clickable workspaces.
        [ className =? "confirm" --> doFloat
        , className =? "file_progress" --> doFloat
        , className =? "dialog" --> doFloat
        , className =? "download" --> doFloat
        , className =? "error" --> doFloat
        , className =? "Gimp" --> doFloat
        , className =? "notification" --> doFloat
        , className =? "pinentry-gtk-2" --> doFloat
        , className =? "splash" --> doFloat
        , className =? "toolbar" --> doFloat
        , className =? "Yad" --> doCenterFloat
        , title =? "Oracle VM VirtualBox Manager" --> doFloat
        , title =? "Order Chain - Market Snapshots" --> doFloat
        , title =? "emacs-run-launcher" --> doFloat
        , title =? "Mozilla Firefox" --> doShift (myWorkspaces !! 1)
        , className =? "Brave-browser" --> doShift (myWorkspaces !! 1)
        , className =? "mpv" --> doShift (myWorkspaces !! 7)
        , className =? "Gimp" --> doShift (myWorkspaces !! 8)
        , className =? "VirtualBox Manager" --> doShift (myWorkspaces !! 4)
        , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat -- Float Firefox Dialog
        , isFullscreen --> doFullFloat
        ]
        <+> namedScratchpadManageHook myScratchPads

myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
    -- (subtitle "Custom Keys":) $ mkNamedKeymap c $
    let subKeys str ks = subtitle' str : mkNamedKeymap c ks
     in subKeys
            ^++^ subKeys
                "Window navigation"
                , ("M-S-,", addName "Rotate all windows except master" $ rotSlavesDown)
                , ("M-S-.", addName "Rotate all windows current stack" $ rotAllDown)
                ]
            -- Scratchpads
            -- Toggle show/hide these programs. They run on a hidden workspace.
            -- When you toggle them to show, it brings them to current workspace.
            -- Toggle them to hide and it sends them back to hidden workspace (NSP).
            ^++^ subKeys
                "Scratchpads"
                [ ("M-s t", addName "Toggle scratchpad terminal" $ namedScratchpadAction myScratchPads "terminal")
                , ("M-s m", addName "Toggle scratchpad mocp" $ namedScratchpadAction myScratchPads "mocp")
                , ("M-<Escape>", addName "Toggle scratchpad calculator" $ namedScratchpadAction myScratchPads "calculator")
                ]
            -- Controls for mocp music player (SUPER-u followed by a key)

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
                        { manageHook = myManageHook <+> manageDocks
                        , handleEventHook = windowedFullscreenFixEventHook <> swallowEventHook (className =? "Alacritty" <||> className =? "st-256color" <||> className =? "XTerm") (return True) <> trayerPaddingXmobarEventHook
                        , modMask = myModMask
                        , terminal = myTerminal
                        , startupHook = myStartupHook
                        , layoutHook = showWName' myShowWNameTheme $ myLayoutHook
                        , workspaces = myWorkspaces
                        , borderWidth = myBorderWidth
                        , normalBorderColor = myNormColor
                        , focusedBorderColor = myFocusColor
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