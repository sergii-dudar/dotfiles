import Color.DoomOne
import qualified Module.Variable as V
import Util.Common
import Util.CommonTwo
import Util.Env.Environment

import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig

import XMonad.Layout.Gaps
import XMonad.Layout.Spacing

unbindKeys :: [String]
unbindKeys =
    [ "M-S-<Return>"
    , "M-p"
    , "M-S-p"
    ]

bindKeys :: [(String, X ())]
bindKeys =
    [ ("M-S-r", spawn "xmonad --recompile &&  xmonad --restart")
    , ("M-<Return>", spawn $ V.terminal V.run)
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