-- Config Functionality Modules
import Control.Monad (when)
import Util.CommonTwo
import Util.Env.Environment

-- Config Modules
import qualified Module.Keybind as K
import qualified Module.Layout as L
import qualified Module.Scratchpad as S
import qualified Module.Startup as SR
import qualified Module.Variable as V
import qualified Module.WinRule as R
import qualified Module.Xmobar as BAR
import qualified Util.Common as U

-- Base
import System.Directory
import System.Exit (exitSuccess)
import System.IO (hClose, hPutStr, hPutStrLn)
import XMonad
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D (..), WSType (..), moveTo, nextScreen, prevScreen, shiftTo)
import qualified XMonad.Actions.FlexibleResize as Flex
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
import XMonad.Hooks.EwmhDesktops -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, doRectFloat, isFullscreen)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory

-- Utilities
import XMonad (XConfig (manageHook), title)
import XMonad.Actions.MostRecentlyUsed (Location (workspace), configureMRU)
import XMonad.Actions.Warp (warpToWindow)
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings, checkKeymap, mkNamedKeymap, removeKeysP)
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
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

mainConfiguration =
    K.applyKeybinds $
        def
            { normalBorderColor = "#535d6c"
            , focusedBorderColor = "#80a0ff"
            , borderWidth = 4
            , modMask = V.keysMod
            , terminal = V.appsTerminal
            , mouseBindings = K.bindMouseKeys
            , manageHook =
                insertPosition End Newer <+> R.manageHookConfig <+> manageHook def <+> S.scratchpadsManageHooks <+> manageDocks
            , layoutHook = avoidStruts $ L.layoutsConfig
            , workspaces = V.workspacesList
            , handleEventHook =
                handleEventHook def
                    <> windowedFullscreenFixEventHook
                    <> trayerPaddingXmobarEventHook
                    <> trayerAboveXmobarEventHook
                    <> swallowEventHook
                        (className =? "com.mitchellh.ghostty" <||> className =? "com.ghostty.group01" <||> className =? "kitty")
                        (return True)
            , startupHook = return () >> checkKeymap mainConfiguration K.bindKeys >> SR.runStartup
            , logHook = refocusLastLogHook >> S.scratchpadsLogHooks
            }

xmobarSB = withEasySB (statusBarProp V.appsXmobarRun (clickablePP BAR.xmobarPPConfig)) K.toggleStrutsKey

main :: IO ()
main = do
    -- xmproc <- spawnPipe "killall xmobar ; xmobar ~/dotfiles/xmobar/.config/xmobar/xmobarrc.hs"
    xmonad
        . configureMRU
        . ewmhFullscreen
        . ewmh
        . xmobarSB
        -- . docks
        $ mainConfiguration

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