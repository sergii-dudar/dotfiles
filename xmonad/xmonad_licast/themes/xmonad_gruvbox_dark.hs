-- IMPORTS

-- base
import XMonad
import XMonad.Config.Desktop
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docksEventHook, docks)
import XMonad.Util.NamedScratchpad
-- actions
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)

-- utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

-- data
import qualified Data.Map as M
import Data.Monoid
import Data.Ratio ((%)) -- for video

-- hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat, doRectFloat)
import XMonad.Util.SpawnOnce
-- layout
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing 
import XMonad.Layout.GridVariants
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End))

-- system
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)

-- variables
myTerminal = "kitty"                   -- default terminal
myScreenshot = "flameshot gui"          -- default screenshot app
myBrowser = "/usr/bin/distrobox-enter -n Arch -- /usr/bin/vivaldi-stable"               -- default browser

myBorderWidth = 3                   -- width of window border
myModMask = mod4Mask                -- default mod key

myNormColor = "#fbf1c7"             -- unfocus window border
myFocusColor = "#fe8019"            -- focus window border
myppCurrent = "#d79921"             -- current workspace in xmobar
myppVisible = "#ebdbb2"             -- visible but not current workspace
myppHidden = "#a3be8c"              -- hidden workspace in xmobar
myppHiddenNoWindows = "#d8dee9"     -- hidden workspace(no windows)
myppTitle = "#b16286"               -- active window title in xmobar
myppUrgent = "#cc241d"              -- urgen workspace

-- workspace
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces = clickable . (map xmobarEscape) $ ["1","2","3","4","5","6","7","8","9"]
--myWorkspaces = clickable . (map xmobarEscape) $ [" \61820 "," \62057 "," \61728 "," \61485 "," \61729 "," \61734 "," \62081 "," \61799 "," \61459 "]
  where
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- window rules
myManageHook = composeAll
    [ className =? "gpick"   --> doFloat
    , className =? "Zenity" --> doFloat
    , title =? "pulsemixer" --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
    , isFullscreen --> doFullFloat
    ]

--startuphook
myStartupHook :: X ()
myStartupHook = do
   spawnOnce "~/.config/xmonad/scripts/autostart.sh"


-- scratchPads
scratchpads :: [NamedScratchpad]
scratchpads = [
-- run htop in xterm, find it by title, use default floating window placement
    NS "term2" "kitty -T term2" (title =? "term2")
        (customFloating $ W.RationalRect (1/12) (1/6) (5/6) (4/6)),

    NS "term1" "kitty -T term1" (title =? "term1")
        (customFloating $ W.RationalRect (1/12) (1/6) (5/6) (4/6)),
    NS "pulse" "kitty -T pulse -e pulsemixer" (title =? "pulse")
        (customFloating $ W.RationalRect (1/12) (1/6) (5/6) (4/6)),
    NS "music" "kitty -T music -e ncspot" (title =? "music")
        (customFloating $ W.RationalRect (1/12) (1/6) (5/6) (4/6)),
    NS "news" "kitty -T news -e newsboat" (title =? "news")
        (customFloating $ W.RationalRect (1/12) (1/6) (5/6) (4/6)),
    NS "ranger" "kitty -T ranger -e ranger" (title =? "ranger")
        (customFloating $ W.RationalRect (1/12) (1/6) (5/6) (4/6))
  ]


-- keybinds
myKeys =
    [ ("M-S-r", spawn "xmonad --recompile; xmonad --restart")                                    -- restart xmonad
    , ("M-S-<Escape>", io exitSuccess)                                       -- quit xmonad
    , ("M-<Return>", spawn myTerminal)                                       -- spawn terminal
    , ("M-d", spawn "rofi -show drun")                                             -- spawn dmenu
    , ("M-q", kill)                                                          -- close focus window
    , ("M-S-q", killAll)                                                     -- kill all window in current workspace
    , ("M-<Tab>", nextScreen)                                                -- Move focus to the next window
    , ("M-S-<Tab>", prevScreen)                                              -- Move focus to the previous window
    , ("M-h", sendMessage Shrink)                                            -- shrink master
    , ("M-l", sendMessage Expand)                                            -- expand master
    , ("M-S-.", sendMessage (IncMasterN (-1)))                               -- deincrement window in master
    , ("M-S-,", sendMessage (IncMasterN 1))                                  -- increment window in master
    , ("M-m", windows W.focusMaster)                                         -- focus master window
    , ("M-j", windows W.focusDown)                                           -- focus next window
    , ("M-k", windows W.focusUp)                                             -- focus prev window
    , ("M-<Space>", sendMessage NextLayout)                                  -- select layout
    , ("M-t", withFocused $ windows . W.sink)                                -- push floating window to tile
    , ("M-S-t", sinkAll)                                                     -- push all floating to tile
    , ("M-<F8>", spawn (myTerminal ++ " -e pulsemixer"))                     -- pulsemixer
    , ("M-", spawn (myTerminal ++ " -e pulsemixer"))                     -- pulsemixer
    , ("M-=", spawn ("pamixer --allow-boost -i 3"))                          -- +vol
    , ("M--", spawn ("pamixer --allow-boost -d 3"))                          -- -vol
    , ("M-S-p", spawn (myTerminal ++ " -e ncmpcpp"))                         -- ncmpcpp
    , ("M1-C-s", spawn (myScreenshot))                         -- ncmpcpp
    , ("M-p", spawn ("mpc toggle"))                                          -- play/pause music
    , ("M-,", spawn ("mpc prev"))                                            -- prev music
    , ("M-.", spawn ("mpc next"))                                            -- next music
    , ("M-[", spawn ("mpc seek -10"))                                        -- seek muisc backward
    , ("M-]", spawn ("mpc seek +10"))                                        -- seek music forward
    , ("<XF86AudioRaiseVolume>", spawn "pamixer --allow-boost -i 3")         -- media-key vol-up
    , ("<XF86AudioLowerVolume>", spawn "pamixer --allow-boost -d 3")         -- media-key vol-down
    , ("M-w", spawn "/usr/bin/distrobox-enter -n Arch -- /usr/bin/vivaldi-stable")  -- firefox
    , ("M-<Escape>", spawn "dprompt \"kill Xorg?\" \"killall Xorg\"")        -- killxorg prompt dmenu
    , ("M-<F12>", spawn (myTerminal ++ " -e tremc"))                         -- torrent
    , ("M-n", namedScratchpadAction scratchpads "term1")
    , ("M-S-n", namedScratchpadAction scratchpads "term2")                          -- newsboat
    , ("M-v", namedScratchpadAction scratchpads "pulse")                          -- calurse
    , ("M-c", namedScratchpadAction scratchpads "ranger")                          -- calurse
    , ("M-m", namedScratchpadAction scratchpads "music")                          -- calurse
    , ("M-b", namedScratchpadAction scratchpads "news")                          -- calurse
    ]




-- layout
myLayout = avoidStruts (tiled ||| full ||| grid ||| bsp)
    where
        -- full
        full = renamed [Replace "F"]
                $ noBorders (Full)
        -- tiled
        tiled = renamed [Replace "||"]
                $ spacingRaw False (Border 10 0 10 0) True (Border 0 10 0 10) True
                $ ResizableTall 1 (3/100) (1/2) []
        -- grid
        grid = renamed [Replace "#"] 
                $ spacingRaw True (Border 10 0 10 0) True (Border 0 10 0 10) True 
                $ Grid (16/10)
        -- bsp
        bsp = renamed [Replace "BSP"]
                $ emptyBSP
        -- default number of windows in master pane
        nmaster = 1
        -- default proportion of screen occupied by master pane
        ratio = 1/2
        -- percent of screen to increment by when resizing panes
        delta = 3/100

-- main
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.xmobarrc"
    xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.xmobarrc"
    xmonad $ ewmh $ docks $ def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+>  namedScratchpadManageHook scratchpads <+> manageHook desktopConfig <+> manageDocks <+> insertPosition End Newer
        , terminal = myTerminal
        , workspaces = myWorkspaces
        , borderWidth = myBorderWidth
        , startupHook = myStartupHook
        , normalBorderColor = myNormColor
        , focusedBorderColor = myFocusColor
        , layoutHook = smartBorders $ myLayout
        , logHook = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP  
                        { ppOutput = \x -> hPutStrLn xmproc0 x  >> hPutStrLn xmproc1 x
                        , ppCurrent = xmobarColor myppCurrent "" . wrap "[" "]"
                        , ppVisible = xmobarColor myppVisible ""
                        , ppHidden = xmobarColor myppHidden "" . wrap "+" ""
                        , ppHiddenNoWindows = xmobarColor  myppHiddenNoWindows ""
                        , ppTitle = xmobarColor myppTitle "" . shorten 30
                        , ppUrgent = xmobarColor myppUrgent "" . wrap "!" "!"
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        , modMask = myModMask
        }
        `additionalKeysP` myKeys
