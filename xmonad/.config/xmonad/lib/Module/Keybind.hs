module Module.Keybind
    ( applyKeybinds
    , bindKeys
    , bindMouseKeys
    ) where

import qualified Data.Map as M

import XMonad

-- Actions
import XMonad.Actions.CycleWS (Direction1D (..), WSType (..), moveTo, nextScreen, prevScreen, shiftTo)
import XMonad.Actions.MostRecentlyUsed (mostRecentlyUsed)
import XMonad.Actions.WithAll (killAll, sinkAll)
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, manageDocks)

-- Config Modules
import qualified Module.Scratchpad as S
import qualified Module.Variable as V
import qualified Util.Common as U

-- Utils

import Util.Common (buildNotifyMsg)
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings, checkKeymap, mkNamedKeymap, removeKeysP)

-- ######################## PUBLIC ##########################

applyKeybinds :: XConfig l -> XConfig l
applyKeybinds defConfig =
    defConfig
        `additionalKeysP` bindKeys
        `removeKeysP` unbindKeys
        `additionalMouseBindings` additionalMouseKeys

bindMouseKeys (XConfig {XMonad.modMask = modm}) =
    M.fromList
        [ ((modm, button1), \w -> focus w >> windows W.shiftMaster)
        , ((modm, button2), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
        , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
        ]

-- ######################## PRIVATE ##########################

additionalMouseKeys :: [((ButtonMask, Button), Window -> X ())]
additionalMouseKeys =
    [ ((mod4Mask, button4), \w -> windows W.focusUp)
    , ((mod4Mask, button5), \w -> windows W.focusDown)
    ]

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
      --  xmonad --recompile &>> ~/tempDump.txt
      ("M-S-r", spawn ("xmonad --recompile ; xmonad --restart && " ++ U.buildNotifyMsg "XMONAD RESTARTED ⭐")) -- recompile and restart xmonad
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
    , ("M1-<Tab>", mostRecentlyUsed [xK_Alt_L, xK_Alt_R] xK_Tab)
    , -- ##############################################################
      -- ##################### SCRATCHPADS ############################
      ("M-y", S.scratchpadsYaziKeyAction)
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