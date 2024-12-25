module Keybinds where

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
    , ("M-p", windows W.focusMaster)                                         -- focus master window
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


