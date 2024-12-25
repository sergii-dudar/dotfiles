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
import Colors.GruvboxDarkShapes

-- utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Actions.Warp

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

myBorderWidth = 3                   -- width of window border
myModMask = mod4Mask                -- default mod key

myNormColor = colorBack             -- unfocus window border
myFocusColor = blue            -- focus window border

-- workspace
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

--myWorkspaces = clickable . (map xmobarEscape) $ [" 1 "," 2 "," 3 "," 4 "," 5 "," 6 "," 7 "," 8 "," 9 "]
myWorkspaces = clickable . (map xmobarEscape) $ [" \984819 "," \62601 "," \58930 "," \987057 "," \983502 "," \984423 "," \59007 "," \58878 "," \984687 "]
--myWorkspaces = clickable . (map xmobarEscape) $ [" \61820 "," \62057 "," \61728 "," \61485 "," \61729 "," \61734 "," \62081 "," \61799 "," \61459 "]
  where
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

--windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

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
    NS "mail" "kitty -T mail -e neomutt" (title =? "mail")
        (customFloating $ W.RationalRect (1/12) (1/6) (5/6) (4/6)),
    NS "yazi" "kitty --class yazi -e yazi" (className =? "yazi")
        (customFloating $ W.RationalRect (1/12) (1/6) (5/6) (4/6))
  ]

-- keybinds
myKeys =
    [ ("M-S-r", spawn "xmonad --recompile; xmonad --restart")    
    , ("M-<Escape>", io exitSuccess)                           
    , ("M-<Return>", spawn myTerminal)                           
    , ("M-d", spawn "rofi -show drun")                           
    , ("M-q", kill)                                              
    , ("M-S-q", killAll)                                         
    , ("M-<Tab>", sequence_ [nextScreen, warpToWindow (1%2) (1%2)])
    , ("M-S-<Tab>", sequence_ [prevScreen, warpToWindow (1%2) (1%2)])
    , ("M-h", sendMessage Shrink)                                 
    , ("M-l", sendMessage Expand)                                 
    , ("M-S-.", sendMessage (IncMasterN (-1)))                    
    , ("M-S-,", sendMessage (IncMasterN 1))                       
    , ("M-p", windows W.focusMaster)                              
    , ("M-j", windows W.focusDown)                                
    , ("M-k", windows W.focusUp)                                  
    , ("M-<Space>", sendMessage NextLayout)                       
    , ("M-t", withFocused $ windows . W.sink)                     
    , ("M-S-t", sinkAll)                                          
-- Scratchpads
    , ("M-n", namedScratchpadAction scratchpads "term1")
    , ("M-S-n", namedScratchpadAction scratchpads "term2")                  
    , ("M-v", namedScratchpadAction scratchpads "pulse")                    
    , ("M-c", namedScratchpadAction scratchpads "yazi")                   
    , ("M-S-m", namedScratchpadAction scratchpads "music")
    , ("M-m", namedScratchpadAction scratchpads "mail")
    , ("M-b", namedScratchpadAction scratchpads "news")                     
-- Apps
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
    xmproc0 <- spawnPipe ("xmobar -x 0 $HOME/.config/xmobar/" ++ colorscheme ++ "-xmobarrc")
    xmproc1 <- spawnPipe ("xmobar -x 1 $HOME/.config/xmobar/" ++ colorscheme ++ "-xmobarrc")
    xmproc2 <- spawnPipe ("xmobar -x 2 $HOME/.config/xmobar/" ++ colorscheme ++ "-xmobarrc")
    xmonad $ ewmh $ docks $ def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> ( isDialog --> doF W.swapUp ) <+> myManageHook <+>  namedScratchpadManageHook scratchpads <+> manageHook desktopConfig <+> manageDocks <+> insertPosition End Newer
        , terminal = myTerminal
        , workspaces = myWorkspaces
        , borderWidth = myBorderWidth
        , startupHook = myStartupHook
        , normalBorderColor = myNormColor
        , focusedBorderColor = myFocusColor
        , layoutHook = smartBorders $ myLayout
        , logHook = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP  
                        { ppOutput = \x -> hPutStrLn xmproc0 x  >> hPutStrLn xmproc1 x >> hPutStrLn xmproc2 x

                        , ppCurrent = xmobarColor yellow "" . wrap "" ""
                        , ppVisible = xmobarColor green ""
                        , ppHidden = xmobarColor magenta "" . wrap "" ""
--                        , ppHiddenNoWindows = xmobarColor  myppHiddenNoWindows ""
                        , ppTitle = xmobarColor yellow "" . shorten 45
                        , ppSep = " - "
                        , ppUrgent = xmobarColor red "" . wrap "!" "!"
                        , ppOrder  = \(ws:_:_) -> [ws]
                        }
        , modMask = myModMask
        }
        `additionalKeysP` myKeys
