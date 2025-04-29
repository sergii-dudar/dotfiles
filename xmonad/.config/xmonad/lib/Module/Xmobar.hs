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
        { -- ppSep = magenta " • "
          -- ppSep = "<fc=" ++ "#bd93f9" ++ "> <fn=1>|</fn> </fc>"
          ppSep = "<hspace=4/>"
        , ppWsSep = ""
        , -- , ppTitleSanitize = xmobarStrip
          ppTitle = yellow . shorten 60
        , -- , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 4 . xmobarBorder "Bottom" "#8be9fd" 4
          ppCurrent =
            blue
                . wrap "" ""
                -- . xmobarBorder "VBoth" "#8be9fd" 4
                . wrap
                    -- ("<fc=#FF0000,#000000><box type=VBoth width=4 mb=2 ml=5 mr=5 color=" ++ "#8be9fd" ++ ">")
                    -- "</box></fc>"
                    -- ("<fc=#FF0000,#000000:0> </fc><fc=#FF0000,#000000:0>")
                    -- "</fc><fc=#FF0000,#000000:0> </fc>"
                    ("<fc=#FF0000,#000000:0>")
                    "</fc>"
        , -- ppCurrent =
          --   blue
          --       . wrap " " ""
          --       . wrap
          --           ("<box type=Bottom width=2 mb=2 color=" ++ "#8be9fd" ++ ">")
          --           "</box>"
          ppHidden = white . wrap "" ""
        , ppHiddenNoWindows = lowWhite . wrap "<fc=#94928F,#2E3440:0>" "</fc>"
        , ppUrgent = red . wrap (yellow "!") (yellow "!")
        , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
        , -- , ppLayout =
          --     ofColor "#d183e8"
          --         . wrap (ofColor "#7C8377" "<fc=#d183e8,#2E3440:0>[<hspace=5/>") (ofColor "#7C8377" "</fc><hspace=5/>]")
          ppLayout =
            wrap
                "<fc=#7C8377,#2E3440:0><hspace=4/>[</fc><fc=#a9a1e1,#2E3440:0><hspace=5/>"
                "<hspace=5/></fc><fc=#7C8377,#2E3440:0>]<hspace=4/></fc><hspace=8/>"
        , ppExtras = [logTitles formatFocused formatUnfocused]
        }
    where
        -- formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
        -- formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow
        -- formatFocused = wrap (white " 👉 ") "" . ofColor "#c678dd" . ppWindow
        formatFocused = wrap (ofColor "#a6d189" "\xf061 ") "" . ofColor "#c678dd" . ppWindow
        formatUnfocused = ofColor "#7C8377" . ppWindow

        -- \| Windows should have *some* title, which should not not exceed a
        -- sane length.
        ppWindow :: String -> String
        ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

        blue, lowWhite, magenta, red, white, yellow :: String -> String
        magenta = xmobarColor "#ff79c6" ""
        blue = xmobarColor "#bd93f9" ""
        white = xmobarColor "#f8f8f2" ""
        yellow = xmobarColor "#f1fa8c" ""
        red = xmobarColor "#ff5555" ""
        lowWhite = xmobarColor "#bbbbbb" ""

ofColor :: String -> String -> String
ofColor color = xmobarColor color ""

{-
,
, -- Hidden workspace
    ppHidden =
    xmobarColor color05 ""
        . wrap
            ("<box type=Top width=2 mt=2 color=" ++ color05 ++ ">")
            "</box>"
        . clickable
, -- Hidden workspaces (no windows)
    ppHiddenNoWindows = xmobarColor color05 "" . clickable
-}

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