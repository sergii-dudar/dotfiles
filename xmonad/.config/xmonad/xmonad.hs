-- Config Functionality Modules

import Control.Monad (when)
import Util.CommonTwo
import Util.Env.Environment

-- Config Modules
import qualified Module.Layout as L
import qualified Module.Scratchpad as S
import qualified Module.Variable as V
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
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap, xmobar, xmobarColor, xmobarPP)
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
import XMonad.Actions.MostRecentlyUsed (Location (workspace))
import XMonad.Actions.Warp (warpToWindow)
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, checkKeymap, mkNamedKeymap, removeKeysP)
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

unbindKeys :: [String]
unbindKeys =
    -- disable default bindings
    [ "M-S-<Return>" -- def: lautch terminal
    , "M-S-q" -- def: quit xmonad
    , "M-q" -- recompile and restart xmonad
    -- , "M-S-p" -- launch gmrun
    , "M-<Space>" -- next layout
    , "M-S-<Tab>" -- prev window
    , "M-S-/" -- Show this help message with the default keybindings",
    -- , "M-."
    -- , "M-,"
    -- , "M-m" -- focus master
    -- , "M-n" -- Resize viewed windows to the correct size
    -- , "M-S-<Space>" -- reset layouts on current screen
    ]

bindKeys :: [(String, X ())]
bindKeys =
    [ -- ##############################################################
      -- ######################## GENERAL #############################
      ("M-S-r", spawn "xmonad --recompile &&  xmonad --restart") -- recompile and restart xmonad
    , ("M-<Return>", spawn V.appsTerminal) -- launch a terminal
    , ("M-<Tab>", sendMessage NextLayout) -- next layout
    , ("M-S-c", kill) -- Close/kill the focused window
    -- , ("M-S-Space", setLayout $ XMonad.layoutHook myConfiguration) -- Reset the layouts on the current workSpace to default"
    -- ##############################################################
    -- ############### WINDOW SIZE, MASTER NUMBER ###################
    , ("M-h", sendMessage Shrink) -- Shrink the master area
    , ("M-l", sendMessage Expand) -- Expand the master area
    , ("M-S-.", sendMessage (IncMasterN 1)) -- Increment the number of windows in the master area
    , ("M-S-,", sendMessage (IncMasterN (-1))) -- Deincrement the number of windows in the master area
    -- ##############################################################
    -- ##################### MULTI MONITORS #########################
    , ("M-.", nextScreen)
    , ("M-,", prevScreen)
    , -- ##############################################################
      -- ########################## FOCUS #############################
      ("M-p", windows W.focusMaster) -- Move focus to the master window
    , ("M-j", windows W.focusDown) -- Move focus to the next window
    , ("M-k", windows W.focusUp) -- Move focus to the previous window
    -- key chorts
    -- , ("M-i j", windows W.focusDown) -- Move focus to the next window
    -- , ("M-i k", windows W.focusUp) -- Move focus to the previous window
    -- ##############################################################
    -- ######################### SWAP ###############################
    , ("M-S-p", windows W.swapMaster) -- Swap the focused window and the master window
    , ("M-S-j", windows W.swapDown) -- Swap the focused window with the next window
    , ("M-S-k", windows W.swapUp) -- Swap the focused window with the previous window
    -- ##############################################################
    -- ######################### OTHER ##############################
    , ("M-b", sendMessage ToggleStruts) -- Toggle the status bar gap (hide xmobar)
    -- , ("M-t", withFocused $ windows . W.sink) -- Push window back into tiling
    , ("M-S-t", sinkAll) -- Push all window back into tiling
    -- ##############################################################
    -- ##################### SCRATCHPADS ############################
    , ("M-y", S.scratchpadsYaziKeyAction)
    , ("M-t", S.scratchpadsTelegramKeyAction)
    , ("M-m", S.scratchpadsYoutubeMusicKeyAction)
    , ("M-g", S.scratchpadsGoogleChatKeyAction)
    , ("M-n", S.scratchpadsNautilusKeyAction)
    , ("M-u", S.scratchpadsMonkeyTypeKeyAction)
    , ("M1-p y", S.scratchpadsYaziKeyAction)
    , ("M1-p t", S.scratchpadsTelegramKeyAction)
    , ("M1-p m", S.scratchpadsYoutubeMusicKeyAction)
    , ("M1-p g", S.scratchpadsGoogleChatKeyAction)
    , ("M1-p n", S.scratchpadsNautilusKeyAction)
    , ("M1-p u", S.scratchpadsMonkeyTypeKeyAction)
    ]

layoutsConfig = L.layoutsTall ||| L.layoutsFull

manageHookConfig =
    composeAll
        [ resource =? "desktop_window" --> doIgnore
        , resource =? "kdesktop" --> doIgnore
        , applyFloatToClass "MPlayer"
        , applyFloatToClass "Gimp"
        , applyFloatToClass "qBittorrent"
        , applyFloatToClass "Arandr"
        , applyFloatToClass "Blueman-manager"
        , applyFloatToClass "Gpick"
        , applyFloatToClass "Kruler"
        , applyFloatToClass "MessageWin" -- kalarm.
        , applyFloatToClass "Sxiv"
        , applyFloatToClass "Tor Browser" -- Needs a fixed window size to avoid fingerprinting by screen size.
        , applyFloatToClass "Wpa_gui"
        , applyFloatToClass "veromix"
        , applyFloatToClass "xtightvncviewer"
        , applyFloatToClass "pavucontrol"
        , applyFloatToClass "gnome-system-monitor"
        , applyFloatToClass "gnome-control-center"
        , applyFloatToClass "gnome-calculator"
        , applyFloatToClass "org.gnome.Characters"
        , applyFloatToClass "org.gnome.clocks"
        , applyFloatToClass "gnome-calendar"
        , applyFloatToClass "Gnome-disks"
        , applyFloatToClass "Nm-connection-editor"
        , applyFloatToClass "ViberPC"
        , applyFloatToClass "vlc"
        , applyFloatToClass "snapshot"
        , applyFloatToClass "Gcolor3"
        , applyFloatToClass "Glate"
        , applyFloatToInstance "disc_ugd"
        , applyFloatToInstance "htop_info"
        , applyFloatToInstance "disc_usabe_info"
        , workspaceToClass 1 "org.wezfurlong.wezterm"
        , workspaceTo 1 "com.ghostty.group01" "ghostty"
        , workspaceToClass 2 "jetbrains-idea"
        , workspaceToClass 2 "Code"
        , workspaceTo 2 "Brave-browser" "brave-browser"
        ]
    where
        applyFloatToClass cname = className =? cname --> doRectFloat U.toRationalRect
        applyFloatToInstance iname = resource =? iname --> doRectFloat U.toRationalRect
        applyFloatTo cname iname = className =? cname <&&> resource =? iname --> doRectFloat U.toRationalRect
        workspaceToClass wnum cname = className =? cname --> doShift (workspacesList !! (wnum - 1))
        workspaceToInstance wnum iname = resource =? iname --> doShift (workspacesList !! (wnum - 1))
        workspaceTo wnum cname iname = className =? cname <&&> resource =? iname --> doShift (workspacesList !! (wnum - 1))

-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
workspacesList = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- myEventHook :: Event -> X ()
-- myEventHook MapNotifyEvent {ev_window = w} = withWindowSet $ \ws -> do
--     whenJust (W.findTag w ws) $ \_ -> do
--         floats <- gets (W.floating . windowset)
--         when (w `M.member` floats) $
--             windows (W.shiftMaster . W.focusWindow w)
-- myEventHook _ = return ()

mainConfiguration =
    def
        { normalBorderColor = "#535d6c"
        , focusedBorderColor = "#80a0ff"
        , borderWidth = 4
        , modMask = V.keysMod
        , terminal = V.appsTerminal
        , manageHook =
            insertPosition End Newer <+> manageHookConfig <+> manageHook def <+> S.scratchpadsManageHooks
        , layoutHook = layoutsConfig
        , workspaces = workspacesList
        , -- , handleEventHook = windowedFullscreenFixEventHook <> swallowEventHook (className =? "Alacritty" <||> className =? "st-256color" <||> className =? "XTerm") (return True) <> trayerPaddingXmobarEventHook
          handleEventHook =
            swallowEventHook
                (className =? "com.mitchellh.ghostty" <||> className =? "com.ghostty.group01" <||> className =? "kitty")
                (return True)
        , startupHook = return () >> checkKeymap mainConfiguration bindKeys
        , logHook = refocusLastLogHook >> S.scratchpadsLogHooks
        }
        `additionalKeysP` bindKeys
        `removeKeysP` unbindKeys

main :: IO ()
main = do
    xmonad $
        ewmhFullscreen $
            ewmh mainConfiguration

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