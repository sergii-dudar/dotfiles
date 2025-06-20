-- Haskell General

import Control.Monad (when)
import qualified Data.Map as M
import Data.Monoid
import Data.Tree
import System.Directory

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
import XMonad

-- Hooks
import XMonad.Hooks.EwmhDesktops -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, manageDocks)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory
import qualified XMonad.StackSet as W

-- Utilities

import Data.Maybe (isNothing)
import XMonad.Actions.MostRecentlyUsed (Location (workspace), configureMRU)
import XMonad.Actions.OnScreen (greedyViewOnScreen, onlyOnScreen)
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Layout.IndependentScreens (withScreens)
import XMonad.Util.ClickableWorkspaces (clickablePP)
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

lastWsToSecondScreenStartupHook :: X ()
lastWsToSecondScreenStartupHook = do
    screens <- gets (W.screens . windowset)
    -- U.notifySend $ "screens " ++ show (length screens)
    when (length screens > 1) $ do
        -- U.notifySend "1231"
        modify $ \xstate ->
            xstate {windowset = onlyOnScreen 1 V.lastWorkspaceId (windowset xstate)}

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
                manageHook def
                    <+> R.manageHookConfig
                    <+> S.scratchpadsManageHooks
            , layoutHook = avoidStruts L.layoutsConfig
            , workspaces = V.workspacesList
            , handleEventHook =
                handleEventHook def
                    <> windowedFullscreenFixEventHook
                    <> trayerPaddingXmobarEventHook
                    <> trayerAboveXmobarEventHook
                    <> swallowEventHook
                        (className =? "com.mitchellh.ghostty" <||> className =? "com.ghostty.group01" <||> className =? "kitty")
                        (return True)
            , startupHook =
                return ()
                    >> checkKeymap mainConfiguration K.bindKeys
                    >> SR.runStartup
                    >> lastWsToSecondScreenStartupHook
            , logHook = refocusLastLogHook >> S.scratchpadsLogHooks
            }

xmobarSB = withEasySB BAR.statusBarConfig K.toggleStrutsKey

main :: IO ()
main = do
    xmonad
        . configureMRU
        . ewmhFullscreen
        . ewmh
        . xmobarSB
        -- . xmobarSBSecond
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