{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ++" #-}

module Module.Xmobar (statusBarConfig, xmobarPPConfig) where

import XMonad

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- Config Modules
import qualified Module.Variable as V
import qualified Util.Common as U

import qualified XMonad.StackSet as W
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)

statusBarConfig :: StatusBarConfig
statusBarConfig =
    statusBarProp V.appsXmobarRun
        . clickablePP
        . filterOutWsPP [scratchpadWorkspaceTag]
        $ xmobarPPConfig

xmobarPPConfig :: PP
xmobarPPConfig =
    def
        { ppSep = "<hspace=3/>"
        , ppWsSep = ""
        , ppTitleSanitize = xmobarStrip
        , ppTitle = color "#f1fa8c" . shorten 60
        , ppCurrent = wrap tagCurrentLeft tagCurrentRight
        , ppHidden = wrap tagNotEmptyLeft tagNotEmptyRight
        , ppHiddenNoWindows = wrap tagEmptyLeft tagEmptyRight
        , ppUrgent = color "#ff5555" . wrap (color "#f1fa8c" "!") (color "#f1fa8c" "!")
        , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
        , ppLayout = wrap layoutLeft layoutRight
        , ppExtras = [logTitles formatFocused formatUnfocused]
        }
    where
        -- formatFocused = wrap (color "#a6d189" "\xf061 ") "" . color "#c678dd" . ppWindow
        -- formatUnfocused = color "#7C8377" . ppWindow
        formatFocused =
            wrap (color "#a6d189" "[<hspace=5/>") (color "#a6d189" "<hspace=5/>]")
                . color "#c678dd"
                . ppWindow
        formatUnfocused =
            wrap " <hspace=5/>" "<hspace=5/> "
                . color "#7C8377"
                . ppWindow
        ppWindow :: String -> String
        ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

tagCurrentLeft =
    concat
        [ "<fc=#a6d189,#44475a:0>"
        , "<hspace=2/>"
        , "<box type=Bottom width=4 ml=0 mr=0 color=#80a0ff>"
        ]
tagCurrentRight =
    concat
        [ "</box>"
        , "<hspace=2/>"
        , "</fc>"
        ]
tagNotEmptyLeft =
    concat
        [ "<fc=#8caaee,#2E3440:0>"
        , "<hspace=2/>"
        , "<box type=Bottom width=4 ml=0 mr=0 color=#535d6c>"
        ]
tagNotEmptyRight =
    concat
        [ "</box>"
        , "<hspace=2/>"
        , "</fc>"
        ]
tagEmptyLeft =
    concat
        [ "<fc=#51576d,#2E3440:0>"
        , "<hspace=2/>"
        ]
tagEmptyRight =
    concat
        [ "<hspace=2/>"
        , "</fc>"
        ]

layoutLeft =
    concat
        [ "<fc=#7C8377,#2E3440:0>"
        , "<hspace=4/>"
        , "["
        , "</fc>"
        , "<fc=#a9a1e1,#2E3440:0>"
        , "<hspace=5/>"
        ]

layoutRight =
    concat
        [ "<hspace=5/>"
        , "</fc>"
        , "<fc=#7C8377,#2E3440:0>"
        , "]"
        , "<hspace=4/>"
        , "</fc><hspace=8/>"
        ]

space :: Int -> String
space pixels = "<hspace=" ++ show pixels ++ "/>"

color :: String -> String -> String
color color = xmobarColor color ""

-- | The 'PP' type allows the user to customize the formatting of
--   status information.

{- data PP = PP { ppCurrent :: WorkspaceId -> String
               -- ^ how to print the tag of the currently focused
               -- workspace
             , ppVisible :: WorkspaceId -> String
               -- ^ how to print tags of visible but not focused
               -- workspaces (xinerama only)
             , ppHidden  :: WorkspaceId -> String
               -- ^ how to print tags of hidden workspaces which
               -- contain windows
             , ppHiddenNoWindows :: WorkspaceId -> String
               -- ^ how to print tags of empty hidden workspaces
             , ppVisibleNoWindows :: Maybe (WorkspaceId -> String)
               -- ^ how to print tags of empty visible workspaces
             , ppUrgent :: WorkspaceId -> String
               -- ^ format to be applied to tags of urgent workspaces.
             , ppRename :: String -> WindowSpace -> String
               -- ^ rename/augment the workspace tag
               --   (note that @WindowSpace -> …@ acts as a Reader monad)
             , ppSep :: String
               -- ^ separator to use between different log sections
               -- (window name, layout, workspaces)
             , ppWsSep :: String
               -- ^ separator to use between workspace tags
             , ppTitle :: String -> String
               -- ^ window title format for the focused window. To display
               -- the titles of all windows—even unfocused ones—check
               -- 'XMonad.Util.Loggers.logTitles'.
             , ppTitleSanitize :: String -> String
              -- ^ escape / sanitizes input to 'ppTitle'
             , ppLayout :: String -> String
               -- ^ layout name format
             , ppOrder :: [String] -> [String]
               -- ^ how to order the different log sections. By
               --   default, this function receives a list with three
               --   formatted strings, representing the workspaces,
               --   the layout, and the current window titles,
               --   respectively. If you have specified any extra
               --   loggers in 'ppExtras', their output will also be
               --   appended to the list.  To get them in the reverse
               --   order, you can just use @ppOrder = reverse@.  If
               --   you don't want to display the current layout, you
               --   could use something like @ppOrder = \\(ws:_:t:_) ->
               --   [ws,t]@, and so on.
             , ppSort :: X ([WindowSpace] -> [WindowSpace])
               -- ^ how to sort the workspaces.  See
               -- "XMonad.Util.WorkspaceCompare" for some useful
               -- sorts.
             , ppExtras :: [X (Maybe String)]
               -- ^ loggers for generating extra information such as
               -- time and date, system load, battery status, and so
               -- on.  See "XMonad.Util.Loggers" for examples, or create
               -- your own!
             , ppOutput :: String -> IO ()
               -- ^ applied to the entire formatted string in order to
               -- output it.  Can be used to specify an alternative
               -- output method (e.g. write to a pipe instead of
               -- stdout), and\/or to perform some last-minute
               -- formatting. Note that this is only used by
               -- 'dynamicLogWithPP'; it won't work with 'dynamicLogString' or
               -- "XMonad.Hooks.StatusBar".
             , ppPrinters :: WSPP
               -- ^ extend workspace types with custom predicates.
               -- Check $predicates for more details.
             } -}