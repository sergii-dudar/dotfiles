import Color.DoomOne
import qualified Module.Variable as V
import Util.Common
import Util.CommonTwo
import Util.Env.Environment

-- Base

import System.Directory
import System.Exit (exitSuccess)
import System.IO (hClose, hPutStr, hPutStrLn)
import XMonad
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D (..), WSType (..), moveTo, nextScreen, prevScreen, shiftTo)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (killAll, sinkAll)

-- Data
import Data.Char (isSpace, toUpper)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Monoid
import Data.Tree

-- Hooks
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.InsertPosition
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
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)
import XMonad.Layout.WindowNavigation

-- Utilities

import XMonad (XConfig (manageHook))
import XMonad.Layout.ResizableTile (ResizableTall (ResizableTall))
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap, removeKeysP)
import XMonad.Util.Hacks
    ( javaHack
    , trayAbovePanelEventHook
    , trayPaddingEventHook
    , trayPaddingXmobarEventHook
    , trayerAboveXmobarEventHook
    , trayerPaddingXmobarEventHook
    , windowedFullscreenFixEventHook
    )
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

unbindKeys :: [String]
unbindKeys =
    [ "M-S-<Return>"
    , "M-S-q"
    , "M-q"
    , "M-p"
    , "M-S-p"
    , "M-<Space>"
    -- , "M-S-<Space>"
    ]

bindKeys :: [(String, X ())]
bindKeys =
    [ ("M-S-r", spawn "xmonad --recompile &&  xmonad --restart")
    , ("M-<Return>", spawn $ V.terminal V.run)
    , ("M-<Tab>", sendMessage NextLayout)
    {- , ("M-<Escape>", io exitSuccess)
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
        , ("M-b", namedScratchpadAction scratchpads "news")                      -}
    -- Apps
    ]

myLayout :: Choose Tall (Choose (Mirror Tall) Full) a
myLayout = tiled ||| Mirror tiled ||| Full
    where
        tiled = Tall nmaster delta ratio
        nmaster = 1 -- Default number of windows in the master pane
        ratio = 1 / 2 -- Default proportion of screen occupied by master pane
        delta = 3 / 100 -- Percent of screen to increment by when resizing panes

-- myConfiguration :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfiguration =
    def
        { modMask = V.modKey V.keys
        , terminal = V.terminal V.run
        , borderWidth = 4
        , manageHook = insertPosition End Newer <+> manageHook def
        , -- , layoutHook = spacingWithEdge 10 myLayout
          --          layoutHook = myLayout
          layoutHook =
            spacingRaw True (Border 4 4 4 4) True (Border 6 6 6 6) True $
                myLayout
        }
        `additionalKeysP` bindKeys
        `removeKeysP` unbindKeys

main :: IO ()
main = do
    xmonad $
        ewmhFullscreen $
            ewmh myConfiguration

{-

data XConfig l = XConfig
    { normalBorderColor  :: !String              -- ^ Non focused windows border color. Default: \"#dddddd\"
    , focusedBorderColor :: !String              -- ^ Focused windows border color. Default: \"#ff0000\"
    , terminal           :: !String              -- ^ The preferred terminal application. Default: \"xterm\"
    , layoutHook         :: !(l Window)          -- ^ The available layouts
    , manageHook         :: !ManageHook          -- ^ The action to run when a new window is opened
    , handleEventHook    :: !(Event -> X All)    -- ^ Handle an X event, returns (All True) if the default handler
                                                 -- should also be run afterwards. mappend should be used for combining
                                                 -- event hooks in most cases.
    , workspaces         :: ![String]            -- ^ The list of workspaces' names
    , modMask            :: !KeyMask             -- ^ the mod modifier
    , keys               :: !(XConfig Layout -> M.Map (ButtonMask,KeySym) (X ()))
                                                 -- ^ The key binding: a map from key presses and actions
    , mouseBindings      :: !(XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ()))
                                                 -- ^ The mouse bindings
    , borderWidth        :: !Dimension           -- ^ The border width
    , logHook            :: !(X ())              -- ^ The action to perform when the windows set is changed
    , startupHook        :: !(X ())              -- ^ The action to perform on startup
    , focusFollowsMouse  :: !Bool                -- ^ Whether window entry events can change focus
    , clickJustFocuses   :: !Bool                -- ^ False to make a click which changes focus to be additionally passed to the window
    , clientMask         :: !EventMask           -- ^ The client events that xmonad is interested in
    , rootMask           :: !EventMask           -- ^ The root events that xmonad is interested in
    , handleExtraArgs    :: !([String] -> XConfig Layout -> IO (XConfig Layout))
                                                 -- ^ Modify the configuration, complain about extra arguments etc. with arguments that are not handled by default
    , extensibleConf     :: !(M.Map TypeRep ConfExtension)
                                                 -- ^ Stores custom config information.
                                                 --
                                                 -- The module "XMonad.Util.ExtensibleConf" in xmonad-contrib
                                                 -- provides additional information and a simple interface for using this.
    }

-}